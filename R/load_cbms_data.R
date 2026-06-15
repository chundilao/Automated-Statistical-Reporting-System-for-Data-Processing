suppressPackageStartupMessages({
  library(yaml)
})

load_cbms_data <- function(config_path = "config.yaml") {
  cfg <- yaml::read_yaml(config_path)
  area_overall <- cfg$area_name[[1]] %||% cfg$area %||% "Total"
  data_env <- new.env(parent = globalenv())

  main_candidates <- c("main.R", cfg$path$script %||% "")
  main_candidates <- unique(main_candidates[nzchar(main_candidates)])
  loaded <- FALSE

  for (f in main_candidates) {
    if (file.exists(f)) {
      try({
        sys.source(f, envir = data_env)
        loaded <- TRUE
      }, silent = TRUE)
      if (loaded) break
    }
  }

  data_obj <- NULL
  for (nm in c("data", "cbms_data", "cbms_tables")) {
    if (exists(nm, envir = data_env, inherits = FALSE)) {
      data_obj <- get(nm, envir = data_env)
      break
    }
  }

  if (is.null(data_obj)) stop("Could not load CBMS data object from main.R or configured script.")
  if (!is.list(data_obj)) stop("Loaded data object is not a list of datasets.")

  list(config = cfg, data = data_obj, area_overall = area_overall)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x
