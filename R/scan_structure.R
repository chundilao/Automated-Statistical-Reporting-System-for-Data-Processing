suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
})

scan_cbms_structure <- function(data_list) {
  if (is.null(data_list) || length(data_list) == 0) {
    return(tibble(
      dataset = character(), variable_name = character(), label = character(),
      description = character(), aliases = character(), value_labels = character(),
      class = character(), n_unique = integer(), sample_values = character()
    ))
  }

  purrr::imap_dfr(data_list, function(df, nm) {
    if (!is.data.frame(df)) return(NULL)
    tibble(
      dataset = nm,
      variable_name = names(df),
      label = purrr::map_chr(df, ~ {
        lb <- attr(.x, "label", exact = TRUE)
        if (is.null(lb)) NA_character_ else as.character(lb)[1]
      }),
      description = NA_character_,
      aliases = NA_character_,
      value_labels = NA_character_,
      class = purrr::map_chr(df, ~ paste(class(.x), collapse = ", ")),
      n_unique = purrr::map_int(df, ~ dplyr::n_distinct(.x, na.rm = TRUE)),
      sample_values = purrr::map_chr(df, ~ {
        vals <- unique(stats::na.omit(as.character(.x)))
        vals <- vals[vals != ""]
        paste(utils::head(vals, 5), collapse = " | ")
      })
    )
  })
}
