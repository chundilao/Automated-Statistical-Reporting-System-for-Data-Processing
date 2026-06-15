suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x

normalize_text <- function(x) {
  x <- tolower(x %||% "")
  x <- gsub("[^a-z0-9_=,:\\n ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

build_cbms_alias_catalog <- function() {
  list(
    cbms_person_record = list(
      sex = c("sex", "male", "female", "gender"),
      relationship = c("relationship to household head", "relation to hh head", "relation to household head", "household head"),
      philid = c("national id", "philid", "philsys", "ownership of national id"),
      employment_status = c("employment", "employment status", "job status"),
      civil_status = c("civil status", "marital status", "married", "single"),
      age = c("age", "years old"),
      household_head_name = c("household head name", "name of household head", "hh head name")
    ),
    cbms_household_record = list(
      toilet_facility = c("toilet", "toilet facility", "toilet facilities", "type of toilet facilities", "sanitation"),
      household_head_name = c("household head name", "name of household head", "hh head name"),
      water_supply = c("water", "source of water"),
      electricity = c("electricity", "power"),
      garbage = c("garbage", "waste disposal")
    )
  )
}

safe_structure_df <- function(structure_df) {
  if (is.null(structure_df) || !is.data.frame(structure_df) || nrow(structure_df) == 0) {
    return(data.frame(
      dataset = character(0),
      variable_name = character(0),
      label = character(0),
      description = character(0),
      aliases = character(0),
      value_labels = character(0),
      sample_values = character(0),
      n_unique = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  needed <- c("dataset", "variable_name", "label", "description", "aliases", "value_labels", "sample_values", "n_unique")
  for (nm in needed) {
    if (!nm %in% names(structure_df)) structure_df[[nm]] <- NA_character_
  }
  structure_df
}

score_structure_match <- function(query_text, structure_df, dataset, candidate_patterns = character(0)) {
  structure_df <- safe_structure_df(structure_df)

  sdf <- structure_df |>
    filter(.data$dataset == !!dataset) |>
    mutate(
      search_blob = normalize_text(paste(
        .data$variable_name,
        .data$label %||% "",
        .data$description %||% "",
        .data$aliases %||% "",
        .data$value_labels %||% "",
        .data$sample_values %||% ""
      )),
      score = 0
    )

  if (nrow(sdf) == 0) return(NULL)

  toks <- unique(strsplit(normalize_text(query_text), " ", fixed = TRUE)[[1]])
  toks <- toks[nchar(toks) >= 3]
  for (tok in toks) {
    sdf$score <- sdf$score + ifelse(str_detect(sdf$search_blob, fixed(tok)), 1, 0)
  }

  for (pat in unique(candidate_patterns)) {
    pat2 <- normalize_text(pat)
    if (!nzchar(pat2)) next
    sdf$score <- sdf$score + ifelse(str_detect(sdf$search_blob, fixed(pat2)), 6, 0)
  }

  sdf <- arrange(sdf, desc(.data$score), .data$n_unique, .data$variable_name)
  if (is.na(sdf$score[[1]]) || sdf$score[[1]] <= 0) return(NULL)
  sdf$variable_name[[1]]
}

resolve_variable <- function(request, structure_df, dataset, fallback = NULL, alias_key = NULL) {
  structure_df <- safe_structure_df(structure_df)
  vars <- structure_df |>
    filter(.data$dataset == !!dataset) |>
    pull(.data$variable_name)

  if (!is.null(request) && nzchar(request) && request %in% vars) return(request)

  patterns <- c(request)
  if (!is.null(alias_key)) {
    patterns <- c(patterns, build_cbms_alias_catalog()[[dataset]][[alias_key]] %||% character(0))
  }

  score_structure_match(request %||% alias_key %||% "", structure_df, dataset, patterns) %||% fallback %||% request
}

extract_section_lines <- function(lines, section_name) {
  idx <- which(grepl(paste0("^", section_name, "\\s*:"), tolower(lines)))
  if (length(idx) == 0) return(character(0))
  start <- idx[1] + 1
  if (start > length(lines)) return(character(0))

  following <- lines[start:length(lines)]
  stop_idx <- which(grepl("^[a-z][a-z_ ]*\\s*:", tolower(following)))
  if (length(stop_idx) > 0) following <- following[seq_len(stop_idx[1] - 1)]
  trimws(gsub("^[*-]\\s*", "", following[nzchar(trimws(following))]))
}

extract_structured_prompt <- function(prompt) {
  lines <- trimws(unlist(strsplit(prompt %||% "", "\\r?\\n")))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return(NULL)

  lower <- tolower(lines)
  has_explicit_sections <- any(grepl("^rows\\s*:", lower)) || any(grepl("^columns\\s*:", lower)) || any(grepl("^filters\\s*:", lower)) || any(grepl("^include\\s*:", lower))
  has_legacy <- any(grepl("^dataset\\s*:", lower)) || any(grepl("^variables\\s*:", lower))
  if (!has_explicit_sections && !has_legacy) return(NULL)

  dataset <- NULL
  if (any(grepl("^dataset\\s*:", lower))) {
    dataset <- trimws(sub("^dataset\\s*:\\s*", "", lines[grep("^dataset\\s*:", lower)[1]], ignore.case = TRUE))
  }

  variables <- extract_section_lines(lines, "variables")
  rows <- extract_section_lines(lines, "rows")
  columns <- extract_section_lines(lines, "columns")
  filters <- extract_section_lines(lines, "filters")
  include <- extract_section_lines(lines, "include")

  list(
    request = lines[1],
    dataset = dataset,
    variables = variables,
    rows = rows,
    columns = columns,
    filters = filters,
    include = include,
    prompt_style = if (has_explicit_sections) "structured_multi" else "structured"
  )
}

infer_dataset_from_text <- function(txt) {
  case_when(
    str_detect(txt, "household|toilet|water|electricity|garbage|dwelling") ~ "cbms_household_record",
    TRUE ~ "cbms_person_record"
  )
}

resolve_row_variable <- function(request, structure_df, dataset, aggregation_level = "barangay") {
  requested <- request %||% aggregation_level
  candidates <- switch(requested,
    barangay = c("barangay", "barangay_name", "barangay_code"),
    city_mun = c("city_mun", "city_mun_name", "city_mun_code", "municipality", "city_municipality"),
    municipality = c("municipality", "city_mun", "city_mun_name", "city_mun_code"),
    province = c("province", "province_name", "province_code"),
    purok = c("purok", "purok_name", "purok_code"),
    c(requested)
  )
  for (cand in candidates) {
    hit <- resolve_variable(cand, structure_df, dataset)
    if (!is.null(hit) && nzchar(hit)) return(hit)
  }
  requested
}

infer_measure <- function(txt) {
  case_when(
    str_detect(txt, "distribution|percentage|percent|share") ~ "percent",
    str_detect(txt, "with |including |show ") ~ "list",
    TRUE ~ "count"
  )
}

parse_filter_expression <- function(x, structure_df, dataset) {
  x <- trimws(x)
  if (!nzchar(x)) return(NULL)

  if (str_detect(x, "=")) {
    parts <- strsplit(x, "=", fixed = TRUE)[[1]]
    lhs_raw <- trimws(parts[1])
    rhs <- trimws(paste(parts[-1], collapse = "="))
    lhs <- resolve_variable(lhs_raw, structure_df, dataset, fallback = lhs_raw)
    if (grepl("^[0-9.]+$", rhs)) {
      return(sprintf("%s == %s", lhs, rhs))
    }
    return(sprintf("%s == '%s'", lhs, rhs))
  }

  x
}

interpret_structured_prompt <- function(sp, structure_df, aggregation_level = "barangay") {
  dataset <- sp$dataset %||% infer_dataset_from_text(normalize_text(paste(c(sp$request, sp$variables, sp$rows, sp$columns, sp$filters, sp$include), collapse = " ")))
  notes <- c(
    if (identical(sp$prompt_style, "structured_multi")) "Multi-section structured prompt detected." else "Structured prompt format detected.",
    paste("Dataset explicitly set to:", dataset)
  )

  if (length(sp$rows) == 0 && length(sp$columns) == 0 && length(sp$variables) > 0) {
    filter_lines <- sp$variables[str_detect(sp$variables, "=")]
    plain_vars <- sp$variables[!str_detect(sp$variables, "=")]

    row_req <- plain_vars[str_detect(tolower(plain_vars), "barangay|city|municip|province|purok")][1] %||% aggregation_level
    row_fields <- resolve_row_variable(row_req, structure_df, dataset, aggregation_level)

    candidate_columns <- setdiff(plain_vars, row_req)
    column_fields <- character(0)
    include_fields <- character(0)

    if (length(candidate_columns) > 0) {
      column_fields <- resolve_variable(candidate_columns[[1]], structure_df, dataset, fallback = candidate_columns[[1]])
    }
    if (length(candidate_columns) > 1) {
      include_fields <- vapply(candidate_columns[-1], resolve_variable, character(1), structure_df = structure_df, dataset = dataset, fallback = "")
      include_fields <- include_fields[nzchar(include_fields)]
    }

    filters <- vapply(filter_lines, parse_filter_expression, character(1), structure_df = structure_df, dataset = dataset)
  } else {
    row_fields <- if (length(sp$rows) > 0) {
      vapply(sp$rows, resolve_row_variable, character(1), structure_df = structure_df, dataset = dataset, aggregation_level = aggregation_level)
    } else {
      resolve_row_variable(aggregation_level, structure_df, dataset, aggregation_level)
    }

    column_fields <- if (length(sp$columns) > 0) {
      vapply(sp$columns, resolve_variable, character(1), structure_df = structure_df, dataset = dataset, fallback = "")
    } else {
      character(0)
    }
    column_fields <- column_fields[nzchar(column_fields)]

    filters <- if (length(sp$filters) > 0) {
      vapply(sp$filters, parse_filter_expression, character(1), structure_df = structure_df, dataset = dataset)
    } else {
      character(0)
    }

    include_fields <- if (length(sp$include) > 0) {
      vapply(sp$include, resolve_variable, character(1), structure_df = structure_df, dataset = dataset, fallback = "")
    } else {
      character(0)
    }
    include_fields <- include_fields[nzchar(include_fields)]
  }

  row_fields <- unique(row_fields[nzchar(row_fields)])
  column_fields <- unique(column_fields[nzchar(column_fields)])
  include_fields <- unique(include_fields[nzchar(include_fields)])

  list(
    prompt_style = sp$prompt_style,
    dataset = dataset,
    rows = row_fields[1] %||% NULL,
    columns = column_fields[1] %||% NULL,
    row_fields = row_fields,
    column_fields = column_fields,
    include_fields = include_fields,
    filters = unname(filters),
    measure = infer_measure(normalize_text(paste(c(sp$request, sp$filters), collapse = " "))),
    table_type = if (length(include_fields) > 0) "detail_summary" else if (length(column_fields) > 1 || length(row_fields) > 1) "crosstab_multi" else "crosstab",
    notes = c(
      notes,
      paste("Resolved rows:", paste(row_fields, collapse = ", ")),
      paste("Resolved columns:", paste(column_fields, collapse = ", ")),
      paste("Resolved filters:", paste(unname(filters), collapse = " ; ")),
      if (length(include_fields) > 0) paste("Resolved include fields:", paste(include_fields, collapse = ", "))
    )
  )
}

interpret_natural_prompt <- function(prompt, structure_df, aggregation_level = "barangay") {
  txt <- normalize_text(prompt)
  dataset <- infer_dataset_from_text(txt)
  row_request <- case_when(
    str_detect(txt, "by purok|per purok") ~ "purok",
    str_detect(txt, "by barangay|per barangay") ~ "barangay",
    str_detect(txt, "by municipality|per municipality|by city|per city") ~ "municipality",
    str_detect(txt, "by province|per province") ~ "province",
    TRUE ~ aggregation_level
  )
  row_fields <- resolve_row_variable(row_request, structure_df, dataset, aggregation_level)

  alias_catalog <- build_cbms_alias_catalog()[[dataset]]
  matched_keys <- character(0)
  for (nm in names(alias_catalog)) {
    pats <- alias_catalog[[nm]]
    if (any(vapply(pats, function(p) str_detect(txt, fixed(normalize_text(p))), logical(1)))) {
      matched_keys <- c(matched_keys, nm)
    }
  }

  column_fields <- character(0)
  include_fields <- character(0)
  if (length(matched_keys) > 0) {
    column_fields <- vapply(matched_keys, function(k) resolve_variable(k, structure_df, dataset, alias_key = k, fallback = ""), character(1))
    column_fields <- unique(column_fields[nzchar(column_fields)])
  }

  list(
    prompt_style = "natural",
    dataset = dataset,
    rows = row_fields,
    columns = column_fields[1] %||% NULL,
    row_fields = unique(c(row_fields)),
    column_fields = column_fields,
    include_fields = include_fields,
    filters = character(0),
    measure = infer_measure(txt),
    table_type = if (length(column_fields) > 1) "crosstab_multi" else "crosstab",
    notes = c("Natural prompt format detected.", paste("Dataset inferred as:", dataset))
  )
}

interpret_cbms_prompt <- function(prompt, structure_df = NULL, aggregation_level = "barangay") {
  structure_df <- safe_structure_df(structure_df)
  sp <- extract_structured_prompt(prompt)
  if (!is.null(sp)) {
    return(interpret_structured_prompt(sp, structure_df, aggregation_level))
  }
  interpret_natural_prompt(prompt, structure_df, aggregation_level)
}
