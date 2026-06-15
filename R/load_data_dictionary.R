suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x

normalize_dictionary_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

standardize_data_dictionary <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble(
    dataset = character(), variable_name = character(), label = character(),
    description = character(), aliases = character(), value_labels = character()
  ))

  names(df) <- normalize_dictionary_names(names(df))

  first_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    x[[1]]
  }

  dataset_col <- first_or_na(intersect(names(df), c("dataset", "table", "file", "record", "record_type")))
  variable_col <- first_or_na(intersect(names(df), c("variable_name", "variable", "field", "var_name", "column_name", "name")))
  label_col <- first_or_na(intersect(names(df), c("label", "question", "variable_label", "field_label", "title")))
  desc_col <- first_or_na(intersect(names(df), c("description", "definition", "notes", "note", "meaning")))
  aliases_col <- first_or_na(intersect(names(df), c("aliases", "alias", "keywords", "keyword", "search_terms")))
  values_col <- first_or_na(intersect(names(df), c("value_labels", "codes", "code_list", "categories", "values")))

  out <- tibble(
    dataset = if (!is.na(dataset_col)) as.character(df[[dataset_col]]) else NA_character_,
    variable_name = if (!is.na(variable_col)) as.character(df[[variable_col]]) else NA_character_,
    label = if (!is.na(label_col)) as.character(df[[label_col]]) else NA_character_,
    description = if (!is.na(desc_col)) as.character(df[[desc_col]]) else NA_character_,
    aliases = if (!is.na(aliases_col)) as.character(df[[aliases_col]]) else NA_character_,
    value_labels = if (!is.na(values_col)) as.character(df[[values_col]]) else NA_character_
  ) |>
    mutate(
      dataset = na_if(trimws(dataset), ""),
      variable_name = trimws(variable_name),
      label = na_if(trimws(label), ""),
      description = na_if(trimws(description), ""),
      aliases = na_if(trimws(aliases), ""),
      value_labels = na_if(trimws(value_labels), "")
    ) |>
    filter(!is.na(variable_name), variable_name != "")

  out
}

read_data_dictionary <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))

  df <- switch(
    ext,
    csv = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    tsv = read.delim(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = {
      if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package `openxlsx` is required to read .xlsx data dictionaries.")
      openxlsx::read.xlsx(path)
    },
    xls = {
      if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package `openxlsx` is required to read .xls/.xlsx data dictionaries.")
      openxlsx::read.xlsx(path)
    },
    rds = readRDS(path),
    stop("Unsupported data dictionary format: ", ext)
  )

  standardize_data_dictionary(as.data.frame(df, stringsAsFactors = FALSE))
}

merge_data_dictionary <- function(structure_df, dictionary_df) {
  if (is.null(dictionary_df) || nrow(dictionary_df) == 0) return(structure_df)

  dict <- dictionary_df |>
    mutate(
      variable_name = trimws(variable_name),
      dataset = trimws(dataset %||% "")
    )

  has_dataset <- any(!is.na(dict$dataset) & dict$dataset != "")

  if (has_dataset) {
    merged <- structure_df |>
      left_join(dict, by = c("dataset", "variable_name"), suffix = c("", "_dict"))
  } else {
    merged <- structure_df |>
      left_join(select(dict, -dataset), by = "variable_name", suffix = c("", "_dict"))
  }

  merged |>
    mutate(
      label = coalesce(label_dict, label),
      description = coalesce(description_dict, description),
      aliases = coalesce(aliases_dict, aliases),
      value_labels = coalesce(value_labels_dict, value_labels)
    ) |>
    select(-any_of(c("label_dict", "description_dict", "aliases_dict", "value_labels_dict")))
}
