library(cli)
cli_rule(left = "Initializing script")

cli_alert_info("Loading R packages")

library(rcdf, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(openxlsx, warn.conflicts = FALSE)
library(yaml, warn.conflicts = FALSE)
library(phscs, warn.conflicts = FALSE)
library(tsg, warn.conflicts = FALSE)

# Load configuration file
cli_alert_info("Reading configuration file")
config <- yaml::read_yaml("config.yaml", readLines.warn = FALSE)
area <- paste(config$area_code, config$area_name)
area_overall <- config$area_name[1]
if(length(config$area_code) > 1 & !is.null(config$area)) {
  area_overall <- config$area
}

# Helper to read .env file
read_env <- function(path = ".env") {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines[!grepl("^\\s*#|^\\s*$", lines)])
  env <- list()
  for (line in lines) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      key <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = "="))
      value <- gsub('^"|"$', '', value)
      env[[key]] <- value
    }
  }
  return(env)
}

# Read environment variables
env <- read_env()

# Load data
cli_alert_info("Loading data")

data <- tryCatch({
  # Try to load real RCDF data
  path_input_data <- config$path$input_data
  
  if(is.null(path_input_data) || !file.exists(path_input_data)) {
    stop("RCDF file not found at configured path")
  }
  
  rcdf_data <- read_rcdf(
    path = path_input_data,
    decryption_key = sapply(paste0('PRIVATE_KEY_PATH', "_", config$area_code), \(x) env[[x]]),
    password = sapply(paste0('PRIVATE_KEY_PW', "_", config$area_code), \(x) env[[x]]),
    return_meta = TRUE
  )
  
  # Process area names
  meta_area <- attributes(rcdf_data)$metadata$area_names
  
  if(is.null(meta_area)) {
    stop("Metadata `area_names` is missing in the RCDF data.")
  }
  
  area_names <- meta_area |>
    dplyr::select(
      city_mun_geo_code = id,
      barangay_geo_code = area_code,
      city_mun = name,
      barangay = area_name
    )
  
  for(record in names(rcdf_data)) {
    if(record == "__data_dictionary") next
    
    rcdf_data[[record]] <- rcdf_data[[record]] |>
      mutate(
        barangay_geo_code = paste0(
          region_code, province_code, city_mun_code, barangay_code
        )
      ) |>
      left_join(area_names, by = "barangay_geo_code") |>
      select(
        any_of("uuid"),
        city_mun_geo_code,
        barangay_geo_code,
        city_mun,
        barangay,
        everything()
      )
  }
  
  # Extract data dictionary
  rcdf_data[['__data_dictionary']] <- NULL
  
  cli_alert_success("Real data loaded successfully")
  
  rcdf_data
  
}, error = function(e) {
  cli_alert_warning(paste("Could not load real data:", e$message))
  cli_alert_info("Loading dummy data instead. Use Settings tab to configure real data.")
  
  list(
    cbms_barangay_record = data.frame(barangay = "Sample"),
    cbms_household_record = data.frame(
      barangay = "Sample", 
      uuid = paste0("HH", 1:5),
      hh_size = c(3, 4, 2, 5, 3),
      number_of_males = c(1, 2, 1, 3, 2),
      number_of_females = c(2, 2, 1, 2, 1),
      number_of_nuclear_families = c(1, 1, 1, 2, 1)
    ),
    cbms_person_record = data.frame(
      barangay = rep("Sample", 17),
      uuid = rep(paste0("HH", 1:5), times = c(3, 4, 2, 5, 3)),
      a03_sex = c(1, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2),
      a05_age = c(25, 30, 5, 45, 38, 12, 8, 55, 60, 28, 22, 3, 65, 70, 35, 40, 15),
      a05_age_group_five_years = c(6, 7, 2, 10, 8, 3, 2, 12, 13, 5, 5, 1, 14, 15, 8, 9, 4),
      e01_work_past_week = c(1, 2, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 1, 2),
      a02_relation_to_hh_head = c(1, 2, 3, 1, 2, 3, 4, 1, 2, 1, 2, 3, 1, 2, 1, 2, 3)
    ),
    cbms_household_record_child_mortality = data.frame(uuid = "HH1"),
    cbms_interview_record = data.frame(uuid = paste0("HH", 1:5)),
    cbms_person_record_tvet = data.frame(uuid = "HH1")
  )
})

# Print data stats
data_stats <- c()
for(i in seq_along(data)) {
  data_stats <- c(
    data_stats,
    "v" = paste0(
      stringr::str_pad(paste0(names(data)[i], " "), pad = " ", side = "right", width = 40),
      cli::style_bold(col_green(
        stringr::str_pad(formatC(nrow(data[[i]]), big.mark = ','), width = 7)
      )), " rows × ",
      cli::style_bold(col_green(
        stringr::str_pad(formatC(ncol(data[[i]]), big.mark = ','), width = 3)
      )), " columns"
    )
  )
}

cli_rule(left = "{area}")
cli_verbatim(data_stats)
cli_text('\n')

# Run additional script if configured
script_path <- config$path$script
if(!is.null(script_path)) {
  if(file.exists(script_path) & str_detect(script_path, "\\.(r|R)$")) {
    source(script_path)
  }
}