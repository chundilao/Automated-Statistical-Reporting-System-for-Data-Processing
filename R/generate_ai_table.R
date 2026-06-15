`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x
}

sanitize_cbms_table <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  has_haven <- requireNamespace("haven", quietly = TRUE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) {
      if (has_haven) {
        df[[nm]] <- as.character(haven::as_factor(x, levels = "default"))
      } else {
        df[[nm]] <- as.character(x)
      }
    } else if (inherits(x, "labelled") || is.factor(x)) {
      df[[nm]] <- as.character(x)
    }
  }
  df
}

build_filter_expression <- function(field, operator, value = NULL) {
  if (is.null(field) || !nzchar(field) || is.null(operator) || !nzchar(operator)) return(NULL)
  if (operator == "is.na") return(sprintf("is.na(%s)", field))
  if (operator == "not.na") return(sprintf("!is.na(%s)", field))
  value <- value %||% ""
  
  if (operator %in% c("contains", "starts_with", "ends_with")) {
    fn <- switch(
      operator,
      contains = "stringr::str_detect",
      starts_with = "stringr::str_starts",
      ends_with = "stringr::str_ends"
    )
    return(sprintf("%s(as.character(%s), %s)", fn, field, shQuote(value)))
  }
  
  if (operator == "between") {
    items <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
    items <- items[nzchar(items)]
    if (length(items) != 2) return(NULL)
    lower <- items[1]
    upper <- items[2]
    return(sprintf("(%s >= %s & %s <= %s)", field, lower, field, upper))
  }
  
  if (operator == "%in%") {
    items <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
    items <- items[nzchar(items)]
    if (length(items) == 0) return(NULL)
    vals <- vapply(
      items,
      function(x) {
        num <- suppressWarnings(as.numeric(x))
        if (!is.na(num)) x else shQuote(x)
      },
      character(1)
    )
    return(sprintf("%s %%in%% c(%s)", field, paste(vals, collapse = ", ")))
  }
  
  num_value <- suppressWarnings(as.numeric(value))
  rhs <- if (!is.na(num_value)) value else shQuote(value)
  sprintf("%s %s %s", field, operator, rhs)
}

normalize_filter_spec <- function(filter_spec) {
  if (is.null(filter_spec) || length(filter_spec) == 0) return(character(0))
  filter_spec <- as.character(filter_spec)
  keep <- !is.na(filter_spec) & nzchar(filter_spec)
  filter_spec[keep]
}

apply_frequency_metrics <- function(long_out, row_vars, metrics) {
  metrics <- unique(metrics %||% "frequency")
  out <- long_out
  
  if ("percentage" %in% metrics) {
    out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(row_vars)))
    out <- dplyr::mutate(
      out,
      Percentage = round(100 * .data$Frequency / sum(.data$Frequency, na.rm = TRUE), 2)
    )
    out <- dplyr::ungroup(out)
  }
  
  out
}

parse_value_labels <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  first_val <- x[[1]]
  if (is.na(first_val) || !nzchar(trimws(as.character(first_val)))) return(NULL)
  
  txt <- as.character(first_val)
  parts <- unlist(strsplit(txt, "[\r\n|;,]+"))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(NULL)
  
  out <- character(0)
  for (part in parts) {
    m <- regexec("^(.+?)\\s*(?:=|:)\\s*(.+)$", part)
    reg <- regmatches(part, m)[[1]]
    if (length(reg) == 3) {
      key <- trimws(reg[2])
      val <- trimws(reg[3])
      if (nzchar(key) && nzchar(val)) out[[key]] <- val
    }
  }
  
  if (length(out) == 0) NULL else out
}

get_value_label_map <- function(df, var_name, structure_df = NULL, dataset_name = NULL) {
  if (!is.null(structure_df) && is.data.frame(structure_df) && "variable_name" %in% names(structure_df)) {
    sdf <- structure_df
    if (!is.null(dataset_name) && nzchar(dataset_name) && "dataset" %in% names(sdf)) {
      sdf <- sdf[sdf$dataset == dataset_name, , drop = FALSE]
    }
    row <- sdf[sdf$variable_name == var_name, , drop = FALSE]
    if (nrow(row) > 0 && "value_labels" %in% names(row)) {
      parsed <- parse_value_labels(row$value_labels[[1]])
      if (!is.null(parsed)) return(parsed)
    }
  }
  
  x <- df[[var_name]]
  lab <- attr(x, "labels", exact = TRUE)
  if (!is.null(lab) && length(lab) > 0) {
    vals <- as.character(unname(lab))
    nms <- names(lab)
    if (!is.null(nms) && length(nms) == length(vals)) {
      return(stats::setNames(as.character(nms), vals))
    }
  }
  
  val_lab <- attr(x, "value.labels", exact = TRUE)
  if (!is.null(val_lab) && length(val_lab) > 0) {
    vals <- as.character(names(val_lab))
    return(stats::setNames(as.character(val_lab), vals))
  }
  
  NULL
}

build_header_value_label <- function(value, label_map = NULL) {
  key_chr <- as.character(value)
  lbl <- NULL
  if (!is.null(label_map) && key_chr %in% names(label_map)) {
    lbl <- label_map[[key_chr]]
  }
  if (is.null(lbl) || !nzchar(lbl)) return(key_chr)
  paste0(key_chr, "_", lbl)
}

standardize_wide_column_names <- function(out, col_values, value_label_map = NULL) {
  header_labels <- list()
  if (length(col_values) == 0) return(list(data = out, header_labels = header_labels))
  
  col_values_chr <- as.character(col_values)
  idx_map <- stats::setNames(seq_along(col_values_chr), col_values_chr)
  renamed <- names(out)
  
  for (i in seq_along(renamed)) {
    nm <- renamed[[i]]
    if (startsWith(nm, "Frequency_")) {
      suffix <- sub("^Frequency_", "", nm)
      if (suffix %in% names(idx_map)) {
        new_name <- paste0("Frequency_", idx_map[[suffix]])
        renamed[[i]] <- new_name
        header_labels[[new_name]] <- build_header_value_label(suffix, value_label_map)
      }
    } else if (startsWith(nm, "Percentage_")) {
      suffix <- sub("^Percentage_", "", nm)
      if (suffix %in% names(idx_map)) {
        new_name <- paste0("Percent_", idx_map[[suffix]])
        renamed[[i]] <- new_name
        header_labels[[new_name]] <- build_header_value_label(suffix, value_label_map)
      }
    }
  }
  
  names(out) <- renamed
  list(data = out, header_labels = header_labels)
}


# ---- NEW: Multiple Variables Distribution table ----
generate_multiple_vars_table <- function(data, spec, area_name_overall, structure_df) {
  if (!spec$dataset %in% names(data)) stop(sprintf("Dataset `%s` not found.", spec$dataset))
  df <- data[[spec$dataset]]
  dataset_name <- spec$dataset
  
  # Apply filters
  filter_spec <- normalize_filter_spec(spec$filters %||% character(0))
  if (length(filter_spec) > 0) {
    if (length(filter_spec) == 1) {
      df <- dplyr::filter(df, !!rlang::parse_expr(filter_spec[[1]]))
    } else {
      for (f in filter_spec) {
        if (!is.null(f) && nzchar(f)) {
          df <- dplyr::filter(df, !!rlang::parse_expr(f))
        }
      }
    }
  }
  
  var_list <- spec$variable_list
  if (length(var_list) == 0) stop("No variables selected.")
  
  missing_vars <- setdiff(var_list, names(df))
  if (length(missing_vars) > 0) {
    stop(sprintf("Variable(s) not found: %s", paste(missing_vars, collapse = ", ")))
  }
  
  # Convert selected variables to factor using haven value labels (if any)
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(var_list), haven::as_factor))
  
  # Prepare variable label lookup from structure_df
  var_labels <- structure_df %>%
    dplyr::filter(.data$dataset == dataset_name, .data$variable_name %in% var_list) %>%
    dplyr::select(.data$variable_name, .data$label) %>%
    dplyr::distinct(.data$variable_name, .keep_all = TRUE)
  lookup <- stats::setNames(var_labels$label, var_labels$variable_name)
  # fallback to variable name if label missing
  get_label <- function(v) { if (v %in% names(lookup) && nzchar(lookup[[v]])) lookup[[v]] else v }
  
  # Grouping
  group_by_barangay <- isTRUE(spec$group_by_barangay)
  if (group_by_barangay && !"barangay" %in% names(df)) stop("'barangay' column not found in dataset.")
  
  # Pivot longer
  id_vars <- if (group_by_barangay) "barangay" else character(0)
  long <- df %>%
    tidyr::pivot_longer(cols = dplyr::all_of(var_list),
                        names_to = "Variable",
                        values_to = "Value",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(Variable = sapply(Variable, get_label))
  
  # Count
  if (group_by_barangay) {
    freq <- long %>%
      dplyr::count(.data$barangay, .data$Variable, .data$Value, name = "Frequency")
    if ("percentage" %in% spec$metrics) {
      freq <- freq %>%
        dplyr::group_by(.data$barangay, .data$Variable) %>%
        dplyr::mutate(Percentage = round(100 * .data$Frequency / sum(.data$Frequency), 2)) %>%
        dplyr::ungroup()
    }
  } else {
    freq <- long %>%
      dplyr::count(.data$Variable, .data$Value, name = "Frequency")
    if ("percentage" %in% spec$metrics) {
      freq <- freq %>%
        dplyr::group_by(.data$Variable) %>%
        dplyr::mutate(Percentage = round(100 * .data$Frequency / sum(.data$Frequency), 2)) %>%
        dplyr::ungroup()
    }
  }
  
  # Pivot wider
  value_cols <- "Frequency"
  if ("percentage" %in% spec$metrics) value_cols <- c(value_cols, "Percentage")
  
  if (group_by_barangay) {
    wide <- freq %>%
      tidyr::pivot_wider(
        id_cols = c(.data$barangay, .data$Variable),
        names_from = .data$Value,
        values_from = dplyr::all_of(value_cols),
        values_fill = 0,
        names_glue = "{.value}_{Value}"
      )
  } else {
    wide <- freq %>%
      tidyr::pivot_wider(
        id_cols = .data$Variable,
        names_from = .data$Value,
        values_from = dplyr::all_of(value_cols),
        values_fill = 0,
        names_glue = "{.value}_{Value}"
      )
  }
  
  header_labels <- list()
  attr(wide, "header_labels") <- header_labels
  sanitize_cbms_table(wide)
}


generate_ai_table <- function(data, spec, area_name_overall = "Total", structure_df = NULL) {
  # Dispatch to multiple‑vars handler
  if (!is.null(spec$table_type) && spec$table_type == "multiple_vars") {
    return(generate_multiple_vars_table(data, spec, area_name_overall, structure_df))
  }
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(rlang)
    library(tidyr)
  })
  
  if (!is.null(spec$join) && isTRUE(spec$join$enabled)) {
    joined_obj <- build_joined_dataset(data, spec$join)
    df <- joined_obj$data
    dataset_name <- spec$join$base_dataset %||% names(data)[[1]]
    dataset_label <- paste0(spec$join$base_dataset, " + ", spec$join$join_dataset)
  } else {
    if (!spec$dataset %in% names(data)) stop(sprintf("Dataset `%s` not found.", spec$dataset))
    df <- data[[spec$dataset]]
    dataset_name <- spec$dataset
    dataset_label <- spec$dataset
  }
  
  filter_spec <- normalize_filter_spec(spec$filters %||% character(0))
  if (length(filter_spec) > 0) {
    if (length(filter_spec) == 1) {
      df <- dplyr::filter(df, !!rlang::parse_expr(filter_spec[[1]]))
    } else {
      for (f in filter_spec) {
        if (!is.null(f) && nzchar(f)) {
          df <- dplyr::filter(df, !!rlang::parse_expr(f))
        }
      }
    }
  }
  
  row_vars <- unique(spec$row_fields %||% spec$rows %||% character(0))
  col_vars <- unique(spec$column_fields %||% spec$columns %||% character(0))
  metrics <- unique(spec$metrics %||% "frequency")
  metrics <- metrics[metrics %in% c("frequency", "percentage")]
  if (length(metrics) == 0) metrics <- "frequency"
  pivot_wide <- isTRUE(spec$pivot_wide)
  
  row_vars <- row_vars[nzchar(row_vars)]
  col_vars <- col_vars[nzchar(col_vars)]
  
  required_vars <- unique(c(row_vars, col_vars))
  missing_vars <- setdiff(required_vars, names(df))
  if (length(missing_vars) > 0) {
    stop(sprintf("Variable(s) not found in `%s`: %s", dataset_label, paste(missing_vars, collapse = ", ")))
  }
  if (length(row_vars) == 0) stop("At least one row field is required.")
  
  long_out <- dplyr::count(
    df,
    dplyr::across(dplyr::all_of(unique(c(row_vars, col_vars)))),
    name = "Frequency"
  )
  long_out <- apply_frequency_metrics(long_out, row_vars = row_vars, metrics = metrics)
  
  if (length(col_vars) == 0 || !pivot_wide) {
    if (length(col_vars) == 0 && length(row_vars) == 1) {
      total_row <- dplyr::summarise(
        long_out,
        dplyr::across(dplyr::where(is.numeric), function(x) sum(x, na.rm = TRUE))
      )
      total_row[[row_vars[[1]]]] <- area_name_overall
      total_row <- total_row[, names(long_out), drop = FALSE]
      long_out <- dplyr::bind_rows(total_row, long_out)
    }
    return(sanitize_cbms_table(long_out))
  }
  
  value_cols <- c()
  if ("frequency" %in% metrics) value_cols <- c(value_cols, "Frequency")
  if ("percentage" %in% metrics && "Percentage" %in% names(long_out)) {
    value_cols <- c(value_cols, "Percentage")
  }
  
  if (length(col_vars) > 1) {
    long_out <- tidyr::unite(long_out, ".column_key", dplyr::all_of(col_vars), sep = " | ", remove = FALSE)
    col_var_name <- ".column_key"
  } else {
    col_var_name <- col_vars[[1]]
  }
  
  out <- tidyr::pivot_wider(
    long_out,
    id_cols = dplyr::all_of(row_vars),
    names_from = dplyr::all_of(col_var_name),
    values_from = dplyr::all_of(value_cols),
    values_fill = 0,
    names_glue = "{.value}_{.name}"
  )
  
  names(out) <- gsub("^Frequency_Frequency_", "Frequency_", names(out))
  names(out) <- gsub("^Percentage_Percentage_", "Percent_", names(out))
  names(out) <- gsub("^Frequency_", "Frequency_", names(out))
  names(out) <- gsub("^Percentage_", "Percent_", names(out))
  
  header_labels <- list()
  if (length(col_vars) == 1) {
    col_var <- col_vars[[1]]
    value_label_map <- get_value_label_map(
      df,
      col_var,
      structure_df = structure_df,
      dataset_name = dataset_name
    )
    
    for (col_name in names(out)) {
      if (startsWith(col_name, "Frequency_")) {
        value_key <- sub("^Frequency_", "", col_name)
        if (nzchar(value_key)) {
          header_labels[[col_name]] <- build_header_value_label(value_key, value_label_map)
        }
      } else if (startsWith(col_name, "Percent_")) {
        value_key <- sub("^Percent_", "", col_name)
        if (nzchar(value_key)) {
          header_labels[[col_name]] <- build_header_value_label(value_key, value_label_map)
        }
      }
    }
  }
  
  freq_cols <- grep("^Frequency_", names(out), value = TRUE)
  if (length(freq_cols) > 0) {
    out$TOTAL <- rowSums(out[, freq_cols, drop = FALSE], na.rm = TRUE)
  }
  
  if ("TOTAL" %in% names(out)) {
    remaining_cols <- setdiff(names(out), c(row_vars, "TOTAL"))
    out <- out[, c(row_vars, "TOTAL", remaining_cols), drop = FALSE]
  }
  
  if (length(row_vars) == 1 && nrow(out) > 0) {
    freq_cols <- grep("^Frequency_", names(out), value = TRUE)
    pct_cols <- grep("^Percent_", names(out), value = TRUE)
    
    total_row <- out[1, , drop = FALSE]
    total_row[,] <- NA
    total_row[[row_vars[[1]]]] <- area_name_overall
    
    for (fc in freq_cols) {
      total_row[[fc]] <- sum(out[[fc]], na.rm = TRUE)
      if (!is.finite(total_row[[fc]]) || total_row[[fc]] < 0) total_row[[fc]] <- 0
    }
    
    grand_total <- if ("TOTAL" %in% names(out)) {
      sum(out[["TOTAL"]], na.rm = TRUE)
    } else {
      sum(unlist(total_row[freq_cols]), na.rm = TRUE)
    }
    if (!is.finite(grand_total) || grand_total <= 0) {
      grand_total <- 1
    }
    total_row[["TOTAL"]] <- grand_total
    
    if (length(freq_cols) == length(pct_cols)) {
      for (i in seq_along(freq_cols)) {
        fc <- freq_cols[i]
        pc <- pct_cols[i]
        if (fc %in% names(total_row) && pc %in% names(total_row)) {
          freq_val <- total_row[[fc]][1]
          total_row[[pc]] <- if (is.finite(freq_val) && freq_val >= 0 && grand_total > 0) {
            round(100 * freq_val / grand_total, 2)
          } else {
            0
          }
        }
      }
    } else {
      for (pc in pct_cols) {
        suffix <- sub(".*_", "", pc)
        matching_freq <- grep(paste0("_", suffix, "$"), freq_cols, value = TRUE)
        if (length(matching_freq) > 0 && matching_freq[1] %in% names(total_row)) {
          freq_val <- total_row[[matching_freq[1]]][1]
          total_row[[pc]] <- if (is.finite(freq_val) && freq_val >= 0 && grand_total > 0) {
            round(100 * freq_val / grand_total, 2)
          } else {
            0
          }
        } else {
          total_row[[pc]] <- 0
        }
      }
    }
    
    total_row <- total_row[, names(out), drop = FALSE]
    out <- dplyr::bind_rows(total_row, out)
  }
  
  if (length(header_labels) > 0) {
    attr(out, "header_labels") <- header_labels
  }
  
  sanitize_cbms_table(out)
}



spec_to_r_code <- function(spec, object_name = "cbms_data$data") {
  # For multiple‑vars table type
  if (!is.null(spec$table_type) && spec$table_type == "multiple_vars") {
    metrics <- unique(spec$metrics %||% "frequency")
    metrics <- metrics[metrics %in% c("frequency", "percentage")]
    if (length(metrics) == 0) metrics <- "frequency"
    
    data_ref <- if (!is.null(spec$join) && isTRUE(spec$join$enabled)) {
      paste0(
        "build_joined_dataset(", object_name, ", list(enabled = TRUE, base_dataset = '", spec$join$base_dataset,
        "', join_dataset = '", spec$join$join_dataset, "', join_type = '", spec$join$join_type,
        "', join_keys = c(", paste(shQuote(spec$join$join_keys), collapse = ", "), "), join_fields = c(",
        paste(shQuote(spec$join$join_fields), collapse = ", "), ")))$data"
      )
    } else {
      paste0(object_name, "[['", spec$dataset, "']]")
    }
    
    filter_spec <- normalize_filter_spec(spec$filters %||% character(0))
    filters_txt <- if (length(filter_spec) > 0) {
      if (length(filter_spec) == 1) {
        paste0(" |>\n  dplyr::filter(", filter_spec[[1]], ")")
      } else {
        paste0(" |>\n  dplyr::filter(", paste(filter_spec, collapse = ", "), ")")
      }
    } else ""
    
    if (isTRUE(spec$group_by_barangay)) {
      group_code <- "  dplyr::group_by(barangay, Variable)"
      pivot_id <- "c(barangay, Variable)"
      id_vars <- "c(barangay, Variable)"
    } else {
      group_code <- "  dplyr::group_by(Variable)"
      pivot_id <- "Variable"
      id_vars <- "Variable"
    }
    
    code <- paste0(
      data_ref, filters_txt,
      " |>\n",
      "  tidyr::pivot_longer(cols = c(", paste(shQuote(spec$variable_list), collapse = ", "), "),\n",
      "    names_to = 'Variable', values_to = 'Value', values_drop_na = TRUE) |>\n",
      "  dplyr::count(", if (isTRUE(spec$group_by_barangay)) "barangay, " else "", "Variable, Value, name = 'Frequency')",
      if ("percentage" %in% metrics) paste0(" |>\n",
                                            "  dplyr::group_by(", if (isTRUE(spec$group_by_barangay)) "barangay, " else "", "Variable) |>\n",
                                            "  dplyr::mutate(Percentage = round(100 * Frequency / sum(Frequency), 2)) |>\n",
                                            "  dplyr::ungroup()"),
      " |>\n",
      "  tidyr::pivot_wider(names_from = Value, values_from = c(Frequency",
      if ("percentage" %in% metrics) ", Percentage",
      "), values_fill = 0, names_glue = '{.value}_{Value}')"
    )
    return(code)
  }
  
  # Original crosstab code
  row_vars <- unique(spec$row_fields %||% spec$rows %||% character(0))
  col_vars <- unique(spec$column_fields %||% spec$columns %||% character(0))
  metrics <- unique(spec$metrics %||% "frequency")
  metrics <- metrics[metrics %in% c("frequency", "percentage")]
  if (length(metrics) == 0) metrics <- "frequency"
  
  data_ref <- if (!is.null(spec$join) && isTRUE(spec$join$enabled)) {
    paste0(
      "build_joined_dataset(", object_name, ", list(enabled = TRUE, base_dataset = '", spec$join$base_dataset,
      "', join_dataset = '", spec$join$join_dataset, "', join_type = '", spec$join$join_type,
      "', join_keys = c(", paste(shQuote(spec$join$join_keys), collapse = ", "), "), join_fields = c(",
      paste(shQuote(spec$join$join_fields), collapse = ", "), ")))$data"
    )
  } else {
    paste0(object_name, "[['", spec$dataset, "']]")
  }
  
  filter_spec <- normalize_filter_spec(spec$filters %||% character(0))
  filters_txt <- if (length(filter_spec) > 0) {
    if (length(filter_spec) == 1) {
      paste0(" |>\n  dplyr::filter(", filter_spec[[1]], ")")
    } else {
      paste0(" |>\n  dplyr::filter(", paste(filter_spec, collapse = ", "), ")")
    }
  } else ""
  
  code <- paste0(
    data_ref, filters_txt,
    " |>\n  dplyr::count(dplyr::across(c(", paste(c(row_vars, col_vars), collapse = ", "), ")), name = 'Frequency')"
  )
  if ("percentage" %in% metrics) {
    code <- paste0(
      code,
      " |>\n  dplyr::group_by(dplyr::across(c(", paste(row_vars, collapse = ", "), ")))",
      " |>\n  dplyr::mutate(Percentage = round(100 * Frequency / sum(Frequency), 2))",
      " |>\n  dplyr::ungroup()"
    )
  }
  if (isTRUE(spec$pivot_wide) && length(col_vars) > 0) {
    code <- paste0(code, "\n# Pivot to wide review table in the app")
  }
  code
}