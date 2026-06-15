suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(later)
})

# Increase file upload limit to 500 MB
options(shiny.maxRequestSize = 500 * 1024^2)

source("R/load_cbms_data.R")
source("R/scan_structure.R")
source("R/load_data_dictionary.R")
source("R/build_joined_dataset.R")
source("R/generate_ai_table.R")
source("R/export_table.R")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x

sanitize_for_display <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  has_haven <- requireNamespace("haven", quietly = TRUE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) {
      df[[nm]] <- if (has_haven) as.character(haven::as_factor(x, levels = "default")) else as.character(x)
    } else if (inherits(x, "labelled") || is.factor(x)) {
      df[[nm]] <- as.character(x)
    }
  }
  df
}

find_dictionary_path <- function(cfg = NULL) {
  candidates <- c(
    cfg$path$data_dictionary %||% "",
    cfg$data_dictionary %||% "",
    "data_dictionary.xlsx",
    "data_dictionary.xls",
    "data_dictionary.csv",
    "dictionary.xlsx",
    "dictionary.csv"
  )
  candidates <- unique(candidates[nzchar(candidates)])
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) return(NULL)
  found[1]
}

build_combined_filter_expression <- function(filter_parts) {
  if (is.null(filter_parts) || length(filter_parts) == 0) return("")
  keep <- !is.na(filter_parts) & nzchar(filter_parts)
  filter_parts <- filter_parts[keep]
  if (length(filter_parts) == 0) return("")
  paste(filter_parts, collapse = "")
}

# ---- Helper to parse RCDF filename ----
parse_rcdf_filename <- function(filename) {
  # Remove path, keep only filename without extension
  base <- tools::file_path_sans_ext(basename(filename))
  
  # Pattern: "CODE NAME" e.g., "0803710 Abuyog, Leyte"
  parts <- strsplit(base, " ")[[1]]
  if (length(parts) >= 2) {
    area_code <- parts[1]
    area_name <- paste(parts[-1], collapse = " ")
    
    # Extract province (after the comma)
    province_split <- strsplit(area_name, ",\\s*")[[1]]
    province <- if (length(province_split) >= 2) province_split[2] else area_name
    
    list(
      area_code = area_code,
      area_name = area_name,
      province = province
    )
  } else {
    list(area_code = "", area_name = base, province = "")
  }
}

cbms_data <- load_cbms_data("config.yaml")
structure_df <- scan_cbms_structure(cbms_data$data)
dictionary_path <- find_dictionary_path(cbms_data$config)
if (!is.null(dictionary_path)) {
  dictionary_df <- tryCatch(read_data_dictionary(dictionary_path), error = function(e) NULL)
  structure_df <- merge_data_dictionary(structure_df, dictionary_df)
}
structure_df <- sanitize_for_display(structure_df)
dataset_names <- names(cbms_data$data)

field_choices_for_dataset <- function(dataset_name) {
  sdf <- structure_df |> dplyr::filter(.data$dataset == dataset_name)
  vars <- unique(sdf$variable_name)
  labels <- sdf$label[match(vars, sdf$variable_name)]
  stats::setNames(vars, ifelse(is.na(labels) | !nzchar(labels), vars, paste0(vars, " — ", labels)))
}

common_join_keys <- function(base_dataset, join_dataset) {
  if (!base_dataset %in% dataset_names || !join_dataset %in% dataset_names) return(character(0))
  intersect(names(cbms_data$data[[base_dataset]]), names(cbms_data$data[[join_dataset]]))
}

# ---- Load credentials ----
credentials <- tryCatch(
  yaml::read_yaml("credentials.yaml"),
  error = function(e) list(password = "admin")
)
VALID_PASSWORD <- credentials$password

# ---- Login UI ----
login_ui <- function() {
  div(
    class = "flex items-center justify-center min-h-screen",
    div(
      class = "bg-white rounded-2xl shadow-xl p-8 w-full max-w-md dark-card",
      h2(class = "text-2xl font-bold text-center mb-6 dark-title", "CBMS Table Generator"),
      div(class = "form-group",
          tags$label("Password", class = "control-label"),
          tags$input(
            id = "login_password",
            type = "password",
            class = "form-control dark-input",
            placeholder = "Enter password"
          )
      ),
      div(class = "mt-4 tw-btn",
          actionButton("login_btn", "Log In", class = "btn-primary w-100")
      ),
      div(class = "mt-3",
          p(class = "text-sm dark-text text-center",
            "Enter the application password to continue.")
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$script(src = "tailwindcss-3.4.17.js"),
    tags$style(HTML("
      /* Base light mode styles */
      body {
        background: #f8fafc;
        transition: background 0.3s ease, color 0.2s ease;
      }
      .app-shell { max-width: 1720px; margin: 0 auto; padding: 24px 18px 32px 18px; }
      .app-card { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 20px; box-shadow: 0 10px 30px rgba(15,23,42,.06); }
      .app-card-soft { background: #ffffff; border: 1px solid #e5e7eb; border-radius: 18px; box-shadow: 0 6px 20px rgba(15,23,42,.04); }
      .section-title { font-size: 12px; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; color: #64748b; margin-bottom: 10px; }
      .control-label { font-size: 12px; margin-bottom: 6px; color:#475569; font-weight:600; }
      .form-group { margin-bottom: 14px; }
      .selectize-control { margin-bottom: 0; }
      .form-control, .selectize-input, .selectize-control.single .selectize-input {
        border-radius: 12px !important;
        border-color: #cbd5e1 !important;
        min-height: 44px;
        box-shadow: none !important;
        transition: background 0.2s, border-color 0.2s;
      }
      .selectize-dropdown, .selectize-input, .form-control { font-size: 14px; }
      .selectize-input.focus, .form-control:focus { border-color:#2563eb !important; box-shadow:0 0 0 3px rgba(37,99,235,.12) !important; }
      .checkbox, .radio { margin-top: 8px; margin-bottom: 8px; }
      .checkbox label, .radio label { color:#334155; }
      .tabbable > .nav > li > a { border-radius: 12px 12px 0 0; color:#334155; font-weight:600; }
      .tabbable > .nav > li.active > a { background:#fff; color:#2563eb; border-color:#e2e8f0 #e2e8f0 transparent #e2e8f0; }
      .tab-content > .tab-pane { background:#fff; border:1px solid #e2e8f0; border-top:none; padding:16px; border-radius:0 0 16px 16px; }
      table.dataTable thead th { background:#f8fafc; }
      .dataTables_wrapper .dt-buttons .dt-button { border-radius:10px !important; border:1px solid #cbd5e1 !important; background:#fff !important; color:#0f172a !important; }
      .filter-preview-box { background:#0f172a; color:#e2e8f0; border-radius:14px; padding:12px 14px; margin-top:8px; white-space:pre-wrap; font-family:Consolas, monospace; font-size:12px; min-height:52px; }
      .tw-btn .btn, .tw-btn .btn-default, .tw-btn .btn-primary {
        border-radius: 12px;
        border: 1px solid #cbd5e1;
        background:#fff;
        color:#0f172a;
        font-weight:600;
        padding: 8px 14px;
      }
      .tw-btn .btn-primary { background:#2563eb; border-color:#2563eb; color:#fff; }
      .tw-btn .btn-primary:hover { background:#1d4ed8; border-color:#1d4ed8; }
      .tw-btn .btn-default:hover, .tw-btn .btn:hover { background:#f8fafc; }
      .tw-btn .btn:focus, .tw-btn .btn-primary:focus, .tw-btn .btn-default:focus { outline:none; box-shadow:0 0 0 3px rgba(37,99,235,.15); }
      .shiny-download-link { display:inline-block; border-radius:12px; padding:10px 14px; background:#0f172a; color:#fff !important; font-weight:600; text-decoration:none !important; }
      .shiny-download-link:hover { background:#020617; color:#fff !important; }
      .filter-card .form-group { margin-bottom: 0; }
      .filter-card .row { margin-left: -6px; margin-right: -6px; }
      .filter-card [class*='col-'] { padding-left: 6px; padding-right: 6px; }
      .dark-text { color: #64748b; }
      .dark-title { color: #0f172a; }
      .dark-input { background: #fff; border-color: #cbd5e1; color: #0f172a; }
      .dark-card { background: #fff; }

      /* Custom toggle switch */
      .theme-toggle {
        width: 44px;
        height: 24px;
        border-radius: 12px;
        background: #cbd5e1;
        appearance: none;
        cursor: pointer;
        position: relative;
        transition: background 0.2s;
      }
      .theme-toggle:checked {
        background: #2563eb;
      }
      .theme-toggle::before {
        content: '';
        position: absolute;
        width: 20px;
        height: 20px;
        border-radius: 50%;
        background: white;
        top: 2px;
        left: 2px;
        transition: transform 0.2s;
      }
      .theme-toggle:checked::before {
        transform: translateX(20px);
      }

      /* Dark mode overrides */
      body.dark {
        background: #0f172a;
        color: #e2e8f0;
      }
      body.dark .dark-text { color: #94a3b8; }
      body.dark .dark-title { color: #f1f5f9; }
      body.dark .dark-input { background: #1e293b; border-color: #334155; color: #e2e8f0; }
      body.dark .dark-card { background: #1e293b; border-color: #334155; }
      body.dark .app-card,
      body.dark .app-card-soft,
      body.dark .tab-content > .tab-pane,
      body.dark .form-control,
      body.dark .selectize-input,
      body.dark .selectize-dropdown,
      body.dark .dataTables_wrapper .dataTables_length label,
      body.dark .dataTables_wrapper .dataTables_filter,
      body.dark .dataTables_wrapper .dataTables_paginate .paginate_button,
      body.dark .dataTables_wrapper .dt-buttons .dt-button{
        background: #1e293b !important;
        border-color: #334155 !important;
        color: #e2e8f0 !important;
      }
      body.dark .app-card-soft {
        background: #1e293b !important;
      }
      body.dark .section-title,
      body.dark .control-label,
      body.dark .checkbox label,
      body.dark .radio label {
        color: #94a3b8 !important;
      }
      body.dark .tabbable > .nav > li > a {
        color: #94a3b8;
      }
      body.dark .tabbable > .nav > li.active > a {
        background: #1e293b;
        color: #60a5fa;
        border-color: #334155 #334155 transparent #334155;
      }
      body.dark .filter-preview-box {
        background: #0f172a;
        color: #cbd5e1;
      }
      body.dark .tw-btn .btn,
      body.dark .tw-btn .btn-default {
        background: #334155;
        border-color: #475569;
        color: #f1f5f9;
      }
      body.dark .tw-btn .btn-primary {
        background: #2563eb;
        border-color: #2563eb;
        color: #fff;
      }
      body.dark .tw-btn .btn-primary:hover {
        background: #1d4ed8;
      }
      body.dark .tw-btn .btn-default:hover {
        background: #475569;
      }
      body.dark .shiny-download-link {
        background: #334155;
        color: #e2e8f0 !important;
      }
      body.dark .shiny-download-link:hover {
        background: #475569;
      }
      body.dark .filter-card {
        background: #0f172a !important;
        border-color: #334155 !important;
      }
      body.dark .filter-card .bg-slate-50 {
        background: #0f172a !important;
      }
      body.dark .filter-card .text-slate-800 {
        color: #e2e8f0 !important;
      }
      body.dark .filter-card .text-slate-500 {
        color: #94a3b8 !important;
      }
      body.dark table.dataTable thead th {
        background: #0f172a !important;
        color: #e2e8f0 !important;
      }
      body.dark table.dataTable tbody tr {
        background: #1e293b !important;
        color: #e2e8f0 !important;
      }
      body.dark table.dataTable tbody tr.even {
        background: #0f172a !important;
      }
      body.dark h1 {
        color: #f1f5f9 !important;
      }
      body.dark .spec-table table,
      body.dark .spec-table td,
      body.dark .spec-table th {
        background: #1e293b !important;
        color: #e2e8f0 !important;
        border-color: #334155 !important;
      }
      body.dark pre {
        background: #0f172a !important;
        color: #e2e8f0 !important;
        border-color: #334155 !important;
      }
      body.dark .modal-content {
        background: #1e293b !important;
        color: #e2e8f0 !important;
        border-color: #334155 !important;
      }
      body.dark .modal-header,
      body.dark .modal-footer {
        border-color: #334155 !important;
      }
      body.dark .modal-header .close,
      body.dark .modal-header .close:hover {
        color: #e2e8f0 !important;
      }
      body.dark .btn-default {
        background: #334155 !important;
        color: #f1f5f9 !important;
        border-color: #475569 !important;
      }
      body.dark .btn-default:hover {
        background: #475569 !important;
      }
      body.dark .w-100 { width: 100%; }
    ")),
    # Fixed dark mode JavaScript
    tags$script(HTML("
      function applyTheme(theme) {
        if (theme === 'dark') {
          $('body').addClass('dark');
          $('#theme-toggle').prop('checked', true);
        } else {
          $('body').removeClass('dark');
          $('#theme-toggle').prop('checked', false);
        }
      }

      $(document).ready(function() {
        var storedTheme = localStorage.getItem('cbms_theme');
        if (!storedTheme) {
          storedTheme = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
        }
        applyTheme(storedTheme);
      });

      $(document).on('shiny:value', function(event) {
        if (event.name === 'main_ui') {
          setTimeout(function() {
            var storedTheme = localStorage.getItem('cbms_theme') || 'light';
            applyTheme(storedTheme);
          }, 150);
        }
      });

      $(document).on('change', '#theme-toggle', function() {
        if (this.checked) {
          $('body').addClass('dark');
          localStorage.setItem('cbms_theme', 'dark');
        } else {
          $('body').removeClass('dark');
          localStorage.setItem('cbms_theme', 'light');
        }
      });
    "))
  ),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  # ---- Authentication state ----
  user_logged_in <- reactiveVal(FALSE)
  
  # ---- Settings reactive values ----
  generated_yaml <- reactiveVal("")
  generated_env <- reactiveVal("")
  
  # ---- Main UI render ----
  output$main_ui <- renderUI({
    if (!user_logged_in()) {
      return(login_ui())
    }
    
    div(
      class = "app-shell",
      div(
        class = "mb-6",
        div(class = "flex flex-col gap-3 lg:flex-row lg:items-end lg:justify-between",
            div(
              h1(class = "text-3xl font-bold tracking-tight text-slate-900 m-0", "CBMS AI Table Generator"),
              p(class = "text-sm text-slate-500 mt-2 mb-0", "Working logic preserved, refreshed with a Tailwind-style interface.")
            ),
            div(class = "flex gap-3 items-center",
                div(class = "text-xs text-slate-500 bg-white border border-slate-200 rounded-2xl px-4 py-3 shadow-sm min-w-[280px]",
                    div(strong("Loaded area: "), cbms_data$area_overall),
                    div(strong("CBMS round: "), cbms_data$config$cbms_round),
                    div(strong("Datasets: "), length(dataset_names))
                ),
                div(class = "flex items-center gap-2",
                    tags$span(class = "text-sm text-slate-600", "🌙"),
                    tags$input(type = "checkbox", id = "theme-toggle", class = "theme-toggle"),
                    tags$span(class = "text-sm text-slate-600", "☀️")
                )
            )
        )
      ),
      div(
        class = "grid grid-cols-1 xl:grid-cols-12 gap-6",
        div(
          class = "xl:col-span-4 2xl:col-span-3",
          div(
            class = "app-card p-5 tw-btn",
            
            checkboxInput("use_manual_code", "Use Manual R Code", FALSE),
            
            conditionalPanel(
              condition = "input.use_manual_code == false",
              div(class = "section-title", "Builder"),
              selectInput("dataset", "Primary Dataset", choices = dataset_names),
              checkboxInput("enable_join", "Join another dataset", FALSE),
              conditionalPanel(
                condition = "input.enable_join == true",
                div(class = "grid grid-cols-1 gap-3",
                    selectInput("join_dataset", "Join Dataset", choices = dataset_names),
                    selectInput("join_type", "Join Type", choices = c("left", "inner", "full", "right"), selected = "left"),
                    uiOutput("join_keys_ui"),
                    uiOutput("join_fields_ui")
                )
              ),
              tags$hr(class = "my-5"),
              div(class = "section-title", "Table layout"),
              selectInput("table_type", "Table type",
                          choices = c("Crosstab (Rows × Columns)" = "crosstab",
                                      "Multiple Variables Distribution" = "multiple_vars"),
                          selected = "crosstab"),
              
              conditionalPanel(
                condition = "input.table_type == 'crosstab'",
                uiOutput("rows_ui"),
                uiOutput("columns_ui"),
                checkboxInput("pivot_wide", "Pivot review table to wide format", TRUE)
              ),
              
              conditionalPanel(
                condition = "input.table_type == 'multiple_vars'",
                uiOutput("multiple_vars_ui"),
                checkboxInput("group_by_barangay", "Group by Barangay", FALSE)
              ),
              
              checkboxGroupInput(
                "metrics",
                "Output columns",
                choices = c("Frequency" = "frequency", "Percentage" = "percentage"),
                selected = "frequency"
              ),
              tags$hr(class = "my-5"),
              div(class = "section-title", "Advanced filters"),
              div(class = "grid grid-cols-1 gap-3",
                  numericInput("filter_count", "Number of filters", value = 0, min = 0, step = 1, width = "100%")
              ),
              div(class = "flex flex-wrap gap-2 items-center mt-2 tw-btn",
                  actionButton("add_filter", "+ Add filter"),
                  actionButton("remove_filter", "− Remove last")
              ),
              p(class = "text-xs text-slate-500 mt-3 mb-3", "Mix AND / OR and add parentheses for grouped conditions."),
              uiOutput("filters_ui"),
              div(class = "mt-4",
                  div(class = "text-sm font-semibold text-slate-700", "Filter preview"),
                  div(class = "filter-preview-box", textOutput("filter_preview", inline = FALSE))
              ),
              div(class = "mt-5 flex flex-wrap gap-3 items-center tw-btn",
                  actionButton("run_builder", "Generate Table", class = "btn-primary"),
                  actionButton("save_spec", "Save Specification", class = "btn-default")
              )
            ),
            
            conditionalPanel(
              condition = "input.use_manual_code == true",
              div(class = "section-title", "Manual R Code"),
              textAreaInput(
                "manual_code",
                "Enter R code to generate table",
                rows = 12,
                placeholder = "# Example:\ndata$cbms_person_record %>%\n  group_by(a05_age_group_five_years, a03_sex) %>%\n  summarise(count = n()) %>%\n  pivot_wider(names_from = a03_sex, values_from = count, values_fill = 0)"
              ),
              div(class = "mt-3 flex flex-wrap gap-3 tw-btn",
                  actionButton("run_manual_code", "Generate Table", class = "btn-primary"),
                  actionButton("save_manual_spec", "Save Specification", class = "btn-default")
              )
            ),
            
            div(class = "mt-4",
                actionButton("open_export_modal", "Export to Excel", class = "btn-primary")
            )
          )
        ),
        div(
          class = "xl:col-span-8 2xl:col-span-9 space-y-6",
          div(
            class = "app-card-soft p-5",
            div(class = "section-title", "Loaded datasets"),
            div(class = "text-sm text-slate-600 leading-6", paste(dataset_names, collapse = ", "))
          ),
          tabsetPanel(
            id = "main_tabs",
            tabPanel("Selected Specification", tableOutput("spec_table")),
            tabPanel("Generated R Code", tags$pre(style = "white-space: pre-wrap;", textOutput("generated_code"))),
            tabPanel("Table Preview", DTOutput("table_preview")),
            tabPanel("Saved Specifications", DTOutput("saved_specs_table")),
            tabPanel("Saved Manual Code", DTOutput("saved_manual_specs_table")),
            tabPanel("Settings",
                     h4("Configure Data Source"),
                     p(class = "text-sm text-slate-500", "Browse for an RCDF file and its private key. The configuration files will be generated automatically."),
                     
                     div(class = "form-group",
                         tags$label("Browse RCDF File", class = "control-label"),
                         fileInput("rcdf_input", NULL, 
                                   accept = ".rcdf",
                                   placeholder = "Example: 0803701 Abuyog, Leyte.rcdf",
                                   width = "100%")
                     ),
                     
                     div(class = "form-group",
                         tags$label("Browse PEM Key", class = "control-label"),
                         fileInput("pem_input", NULL,
                                   accept = ".pem",
                                   placeholder = "Select private key file",
                                   width = "100%")
                     ),
                     
                     div(class = "form-group",
                         tags$label("Private Key Password", class = "control-label"),
                         passwordInput("pem_password_input", NULL, 
                                       placeholder = "Enter private key password",
                                       width = "100%")
                     ),
                     
                     tags$hr(),
                     
                     div(class = "section-title mt-4", "Generated config.yaml"),
                     div(class = "filter-preview-box", 
                         style = "font-size:13px; max-height:400px; overflow-y:auto;",
                         verbatimTextOutput("config_yaml_preview")),
                     
                     div(class = "section-title mt-4", "Generated .env"),
                     div(class = "filter-preview-box",
                         style = "font-size:13px; max-height:200px; overflow-y:auto;",
                         verbatimTextOutput("env_preview")),
                     
                     div(class = "mt-4 tw-btn",
                         actionButton("save_settings", "Save & Reload App", class = "btn-primary")
                     )
            ),
            tabPanel(
              "Dataset Structure",
              div(class = "mb-3", selectInput("dataset_filter", "Filter dataset", choices = c("All", dataset_names))),
              DTOutput("structure_table")
            )
          )
        )
      )
    )
  })
  
  # ---- Login handler ----
  observeEvent(input$login_btn, {
    if (input$login_password == VALID_PASSWORD) {
      user_logged_in(TRUE)
    } else {
      showNotification("Incorrect password", type = "error")
    }
  })
  
  # ---- Settings: auto-generate YAML and ENV ----
  observe({
    req(input$rcdf_input)
    
    filename <- input$rcdf_input$name
    info <- parse_rcdf_filename(filename)
    
    rcdf_dir <- "data/rcdf"
    dir.create(rcdf_dir, showWarnings = FALSE, recursive = TRUE)
    rcdf_dest <- file.path(rcdf_dir, filename)
    file.copy(input$rcdf_input$datapath, rcdf_dest, overwrite = TRUE)
    
    yaml_content <- sprintf(
      "cbms_round: '2024'\narea_code:\n  - \"%s\"\narea_name:\n  - \"%s\"\narea: \"%s\"\naggregation:\n  level: \"barangay\"\n  label: \"Barangay\"\ninput_data_type: \"rcdf\"\npath:\n  input_data: \"%s\"\n  output: \"\"\n  script: \"\"\n",
      info$area_code, info$area_name, info$province, rcdf_dest
    )
    generated_yaml(yaml_content)
    
    if (!is.null(input$pem_input)) {
      pem_dir <- file.path("data", info$area_code, "keys")
      dir.create(pem_dir, showWarnings = FALSE, recursive = TRUE)
      pem_dest <- file.path(pem_dir, input$pem_input$name)
      file.copy(input$pem_input$datapath, pem_dest, overwrite = TRUE)
      
      password <- if (nzchar(input$pem_password_input)) input$pem_password_input else "changeme"
      
      env_content <- sprintf(
        "PRIVATE_KEY_PATH_%s=\"%s\"\nPRIVATE_KEY_PW_%s=\"%s\"\n",
        info$area_code, pem_dest, info$area_code, password
      )
      generated_env(env_content)
    }
  })
  
  output$config_yaml_preview <- renderText({
    if (nzchar(generated_yaml())) generated_yaml() else "Browse an RCDF file to generate config.yaml"
  })
  
  output$env_preview <- renderText({
    if (nzchar(generated_env())) generated_env() else "Browse a PEM key to generate .env"
  })
  
  # ---- Save & Reload ----
  observeEvent(input$save_settings, {
    req(nzchar(generated_yaml()))
    writeLines(generated_yaml(), "config.yaml")
    if (nzchar(generated_env())) {
      writeLines(generated_env(), ".env")
    }
    showNotification("Settings saved. Reloading app in 1 second...", type = "message")
    later::later(function() {
      session$reload()
    }, 1)
  })
  
  # ---- Rest of server (specifications, builder, manual code, export) ----
  SPECS_FILE <- "saved_tabulation_specs.rds"
  MANUAL_SPECS_FILE <- "saved_manual_specs.rds"
  
  saved_specs <- reactiveValues(specs = list(), loaded = FALSE)
  saved_manual_specs <- reactiveValues(specs = list(), loaded = FALSE)
  
  load_specs_from_file <- function() {
    if (file.exists(SPECS_FILE)) {
      tryCatch({
        loaded <- readRDS(SPECS_FILE)
        saved_specs$specs <- if (is.list(loaded)) loaded else list()
        saved_specs$loaded <- TRUE
      }, error = function(e) {
        saved_specs$specs <- list()
        saved_specs$loaded <- TRUE
      })
    } else {
      saved_specs$specs <- list()
      saved_specs$loaded <- TRUE
    }
  }
  
  load_manual_specs_from_file <- function() {
    if (file.exists(MANUAL_SPECS_FILE)) {
      tryCatch({
        loaded <- readRDS(MANUAL_SPECS_FILE)
        saved_manual_specs$specs <- if (is.list(loaded)) loaded else list()
        saved_manual_specs$loaded <- TRUE
      }, error = function(e) {
        saved_manual_specs$specs <- list()
        saved_manual_specs$loaded <- TRUE
      })
    } else {
      saved_manual_specs$specs <- list()
      saved_manual_specs$loaded <- TRUE
    }
  }
  
  persist_specs <- function() {
    tryCatch({ saveRDS(saved_specs$specs, SPECS_FILE) }, error = function(e) {})
  }
  
  persist_manual_specs <- function() {
    tryCatch({ saveRDS(saved_manual_specs$specs, MANUAL_SPECS_FILE) }, error = function(e) {})
  }
  
  observeEvent(TRUE, {
    load_specs_from_file()
    load_manual_specs_from_file()
  }, once = TRUE)
  
  update_ui_from_spec <- function(spec) {
    updateSelectInput(session, "dataset", selected = spec$dataset)
    updateCheckboxInput(session, "enable_join", value = spec$join$enabled)
    if (spec$join$enabled) {
      updateSelectInput(session, "join_dataset", selected = spec$join$join_dataset)
      updateSelectInput(session, "join_type", selected = spec$join$join_type)
    }
    updateCheckboxGroupInput(session, "metrics", selected = intersect(spec$metrics %||% "frequency", c("frequency", "percentage")))
    updateSelectInput(session, "table_type", selected = spec$table_type %||% "crosstab")
    
    filter_n <- length(spec$filter_details)
    updateNumericInput(session, "filter_count", value = filter_n)
    
    later::later(function() {
      if (spec$table_type == "multiple_vars") {
        updateSelectizeInput(session, "multiple_variables", selected = spec$variable_list %||% character(0))
        updateCheckboxInput(session, "group_by_barangay", value = isTRUE(spec$group_by_barangay))
      } else {
        updateSelectizeInput(session, "row_fields", selected = spec$row_fields)
        updateSelectizeInput(session, "column_fields", selected = spec$column_fields)
        updateCheckboxInput(session, "pivot_wide", value = spec$pivot_wide)
      }
      
      for (i in seq_along(spec$filter_details)) {
        fd <- spec$filter_details[[i]]
        updateSelectizeInput(session, paste0("filter_field_", i), selected = fd$field)
        updateSelectInput(session, paste0("filter_op_", i), selected = fd$op)
        updateTextInput(session, paste0("filter_value_", i), value = fd$value %||% "")
        updateNumericInput(session, paste0("filter_open_", i), value = fd$open)
        updateNumericInput(session, paste0("filter_close_", i), value = fd$close)
        if (i > 1) updateSelectInput(session, paste0("filter_logic_", i), selected = fd$logic)
      }
      
      if (spec$join$enabled) {
        later::later(function() {
          updateSelectizeInput(session, "join_keys", selected = spec$join$join_keys)
          updateSelectizeInput(session, "join_fields", selected = spec$join$join_fields)
        }, delay = 1.0)
      }
    }, delay = 2.0)
  }
  
  update_ui_from_manual_spec <- function(spec) {
    updateCheckboxInput(session, "use_manual_code", value = TRUE)
    updateTextAreaInput(session, "manual_code", value = spec$code %||% "")
  }
  
  observeEvent(input$dataset, {
    fallback <- setdiff(dataset_names, input$dataset)[1]
    updateSelectInput(session, "join_dataset", selected = fallback %||% input$dataset)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_filter, {
    current_n <- suppressWarnings(as.integer(input$filter_count %||% 0))
    if (is.na(current_n) || current_n < 0) current_n <- 0
    updateNumericInput(session, "filter_count", value = current_n + 1)
  })
  
  observeEvent(input$remove_filter, {
    current_n <- suppressWarnings(as.integer(input$filter_count %||% 0))
    if (is.na(current_n) || current_n < 0) current_n <- 0
    updateNumericInput(session, "filter_count", value = max(0, current_n - 1))
  })
  
  available_builder_fields <- reactive({
    req(input$dataset)
    base_names <- names(cbms_data$data[[input$dataset]])
    base_choices <- field_choices_for_dataset(input$dataset)
    
    if (!isTRUE(input$enable_join) || is.null(input$join_dataset) || !nzchar(input$join_dataset)) {
      return(base_choices)
    }
    
    join_keys <- input$join_keys %||% character(0)
    join_fields <- input$join_fields %||% character(0)
    rename_map <- resolve_join_field_names(base_names, join_keys, join_fields)
    
    join_sdf <- structure_df |> dplyr::filter(.data$dataset == input$join_dataset)
    join_labels <- join_sdf$label[match(join_fields, join_sdf$variable_name)]
    join_out_names <- unname(rename_map[join_fields])
    join_display <- ifelse(is.na(join_labels) | !nzchar(join_labels), join_out_names, paste0(join_out_names, " — ", join_labels, " [", input$join_dataset, "]"))
    join_choices <- stats::setNames(join_out_names, join_display)
    
    c(base_choices, join_choices)
  })
  
  output$join_keys_ui <- renderUI({
    req(input$dataset, input$join_dataset)
    keys <- common_join_keys(input$dataset, input$join_dataset)
    selectizeInput("join_keys", "Join Keys", choices = keys, selected = keys, multiple = TRUE)
  })
  
  output$join_fields_ui <- renderUI({
    req(input$join_dataset)
    choices <- field_choices_for_dataset(input$join_dataset)
    selected <- head(setdiff(names(choices), input$join_keys %||% character(0)), 5)
    selectizeInput("join_fields", "Fields to bring from Join Dataset", choices = choices, selected = selected, multiple = TRUE)
  })
  
  output$rows_ui <- renderUI({
    selectizeInput("row_fields", "Rows", choices = available_builder_fields(), multiple = TRUE)
  })
  
  output$columns_ui <- renderUI({
    selectizeInput("column_fields", "Columns", choices = available_builder_fields(), multiple = TRUE)
  })
  
  output$multiple_vars_ui <- renderUI({
    req(input$dataset)
    choices <- field_choices_for_dataset(input$dataset)
    selectizeInput("multiple_variables", "Select variables",
                   choices = choices, multiple = TRUE,
                   options = list(placeholder = "Choose variables..."))
  })
  
  output$filters_ui <- renderUI({
    n <- max(0, as.integer(input$filter_count %||% 0))
    if (n == 0) return(NULL)
    choices <- available_builder_fields()
    ops <- c("==" = "==", "!=" = "!=", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=",
             "Between" = "between", "%in%" = "%in%", "Contains" = "contains",
             "Starts with" = "starts_with", "Ends with" = "ends_with",
             "is.na" = "is.na", "not.na" = "not.na")
    
    tagList(lapply(seq_len(n), function(i) {
      div(
        class = "filter-card bg-slate-50 border border-slate-200 rounded-2xl p-4 mb-3",
        div(class = "flex items-center justify-between mb-3",
            div(class = "flex items-center gap-2",
                div(class = "w-8 h-8 rounded-full bg-blue-600 text-white flex items-center justify-center text-sm font-semibold", i),
                tags$div(class = "font-semibold text-slate-800", paste("Filter", i))
            ),
            if (i == 1) tags$span(class = "text-xs font-semibold text-slate-500 uppercase tracking-wide", "Start")
        ),
        fluidRow(
          column(6, numericInput(paste0("filter_open_", i), "(", value = 0, min = 0, step = 1, width = "100%")),
          column(6, numericInput(paste0("filter_close_", i), ")", value = 0, min = 0, step = 1, width = "100%"))
        ),
        tags$div(class = "mt-3"),
        fluidRow(
          column(5, selectizeInput(paste0("filter_field_", i), "Field", choices = choices, multiple = FALSE)),
          column(3, selectInput(paste0("filter_op_", i), "Operator", choices = ops, selected = "==")),
          column(4, textInput(paste0("filter_value_", i), "Value", placeholder = "e.g. 1, Male,Female, or 5,24"))
        ),
        fluidRow(
          column(8, if (i == 1) {
            textInput(paste0("filter_logic_label_", i), "Logic", value = "Start", width = "100%")
          } else {
            selectInput(paste0("filter_logic_", i), "Logic", choices = c("AND" = "&", "OR" = "|"), selected = "&")
          }),
          column(4, div(class = "pt-4 tw-btn", actionButton(paste0("clear_filter_", i), "Clear", class = "btn")))
        ),
        div(class = "text-xs text-slate-500 mt-2", "Tip: use Between with min,max. For is.na and not.na, the Value can be left blank.")
      )
    }))
  })
  
  lapply(1:100, function(i) {
    observeEvent(input[[paste0("clear_filter_", i)]], {
      updateNumericInput(session, paste0("filter_open_", i), value = 0)
      updateNumericInput(session, paste0("filter_close_", i), value = 0)
      updateTextInput(session, paste0("filter_value_", i), value = "")
      if (!is.null(input[[paste0("filter_field_", i)]])) updateSelectizeInput(session, paste0("filter_field_", i), selected = "")
      if (!is.null(input[[paste0("filter_op_", i)]])) updateSelectInput(session, paste0("filter_op_", i), selected = "==")
      if (i > 1 && !is.null(input[[paste0("filter_logic_", i)]])) updateSelectInput(session, paste0("filter_logic_", i), selected = "&")
    }, ignoreInit = TRUE)
  })
  
  build_filter_preview <- reactive({
    filter_parts <- character(0)
    n <- max(0, as.integer(input$filter_count %||% 0))
    for (i in seq_len(n)) {
      field <- input[[paste0("filter_field_", i)]]
      op <- input[[paste0("filter_op_", i)]]
      value <- input[[paste0("filter_value_", i)]]
      expr <- build_filter_expression(field, op, value)
      if (!is.null(expr) && nzchar(expr)) {
        open_n <- max(0, as.integer(input[[paste0("filter_open_", i)]] %||% 0))
        close_n <- max(0, as.integer(input[[paste0("filter_close_", i)]] %||% 0))
        logic <- if (i == 1) "" else (input[[paste0("filter_logic_", i)]] %||% "&")
        filter_parts <- c(filter_parts, paste0(
          if (i > 1) paste0(" ", logic, " ") else "",
          paste(rep("(", open_n), collapse = ""), expr, paste(rep(")", close_n), collapse = "")
        ))
      }
    }
    build_combined_filter_expression(filter_parts)
  })
  
  output$filter_preview <- renderText({
    fp <- build_filter_preview()
    if (length(fp) == 0 || !nzchar(fp)) "No active filters" else fp
  })
  
  builder_spec <- eventReactive(input$run_builder, {
    req(input$dataset)
    filter_details <- list()
    n <- max(0, as.integer(input$filter_count %||% 0))
    filter_parts <- character(0)
    
    for (i in seq_len(n)) {
      field <- input[[paste0("filter_field_", i)]]
      op <- input[[paste0("filter_op_", i)]]
      value <- input[[paste0("filter_value_", i)]]
      expr <- build_filter_expression(field, op, value)
      
      open_n <- max(0, as.integer(input[[paste0("filter_open_", i)]] %||% 0))
      close_n <- max(0, as.integer(input[[paste0("filter_close_", i)]] %||% 0))
      logic <- if (i == 1) "" else (input[[paste0("filter_logic_", i)]] %||% "&")
      
      if (!is.null(expr) && nzchar(expr)) {
        filter_parts <- c(filter_parts, paste0(
          if (i > 1) paste0(" ", logic, " ") else "",
          paste(rep("(", open_n), collapse = ""), expr, paste(rep(")", close_n), collapse = "")
        ))
      }
      
      filter_details[[i]] <- list(field = field %||% "", op = op %||% "==", value = value %||% "",
                                  open = open_n, close = close_n, logic = logic)
    }
    
    combined_filter <- build_combined_filter_expression(filter_parts)
    if (length(combined_filter) == 0 || is.na(combined_filter[[1]]) || !nzchar(combined_filter[[1]])) {
      combined_filter <- character(0)
    } else { combined_filter <- combined_filter[[1]] }
    
    table_type <- input$table_type %||% "crosstab"
    if (table_type == "multiple_vars") {
      variable_list <- input$multiple_variables %||% character(0)
      row_fields <- character(0); column_fields <- character(0)
      pivot_wide <- TRUE; group_by_barangay <- isTRUE(input$group_by_barangay)
    } else {
      variable_list <- character(0)
      row_fields <- input$row_fields %||% character(0)
      column_fields <- input$column_fields %||% character(0)
      pivot_wide <- isTRUE(input$pivot_wide); group_by_barangay <- FALSE
    }
    
    list(dataset = input$dataset, table_type = table_type, variable_list = variable_list,
         row_fields = row_fields, column_fields = column_fields, filters = combined_filter,
         metrics = intersect(input$metrics %||% "frequency", c("frequency", "percentage")),
         pivot_wide = pivot_wide, group_by_barangay = group_by_barangay, table_format = "standard",
         join = list(enabled = isTRUE(input$enable_join), base_dataset = input$dataset,
                     join_dataset = input$join_dataset, join_type = input$join_type,
                     join_keys = input$join_keys %||% character(0), join_fields = input$join_fields %||% character(0)),
         filter_details = filter_details)
  })
  
  table_reactive <- eventReactive(input$run_builder, {
    generate_ai_table(cbms_data$data, builder_spec(), area_name_overall = cbms_data$area_overall, structure_df = structure_df)
  })
  
  manual_table <- eventReactive(input$run_manual_code, {
    req(input$manual_code)
    env <- new.env(parent = .GlobalEnv)
    env$data <- cbms_data$data; env$area_name_overall <- cbms_data$area_overall; env$structure_df <- structure_df
    tryCatch({
      result <- eval(parse(text = input$manual_code), envir = env)
      if (!is.data.frame(result)) { showNotification("The result must be a data.frame.", type = "error"); return(NULL) }
      result
    }, error = function(e) { showNotification(paste("Error:", e$message), type = "error", duration = 10); NULL })
  })
  
  current_preview_table <- reactive({ if (isTRUE(input$use_manual_code)) manual_table() else table_reactive() })
  
  output$spec_table <- renderTable({
    if (isTRUE(input$use_manual_code)) return(data.frame(Information = "Manual code – no specification."))
    spec <- builder_spec()
    if (spec$table_type == "multiple_vars") {
      data.frame(field = c("dataset", "join_enabled", "join_dataset", "join_keys", "join_fields", "variables", "group_by_barangay", "filters", "metrics"),
                 value = c(spec$dataset, spec$join$enabled, spec$join$join_dataset %||% "",
                           paste(spec$join$join_keys %||% character(0), collapse = ", "),
                           paste(spec$join$join_fields %||% character(0), collapse = ", "),
                           paste(spec$variable_list, collapse = ", "), spec$group_by_barangay,
                           paste(spec$filters, collapse = " ; "), paste(spec$metrics, collapse = ", ")),
                 stringsAsFactors = FALSE)
    } else {
      data.frame(field = c("dataset", "join_enabled", "join_dataset", "join_keys", "join_fields", "rows", "columns", "filters", "metrics", "table_format", "pivot_wide"),
                 value = c(spec$dataset, spec$join$enabled, spec$join$join_dataset %||% "",
                           paste(spec$join$join_keys %||% character(0), collapse = ", "),
                           paste(spec$join$join_fields %||% character(0), collapse = ", "),
                           paste(spec$row_fields, collapse = ", "), paste(spec$column_fields, collapse = ", "),
                           paste(spec$filters, collapse = " ; "), paste(spec$metrics, collapse = ", "),
                           spec$table_format %||% "standard", spec$pivot_wide),
                 stringsAsFactors = FALSE)
    }
  }, bordered = TRUE, width = "100%")
  
  output$generated_code <- renderText({
    if (isTRUE(input$use_manual_code)) input$manual_code %||% "" else { req(builder_spec()); spec_to_r_code(builder_spec()) }
  })
  
  output$table_preview <- renderDT({
    req(current_preview_table())
    tbl <- sanitize_for_display(current_preview_table())
    datatable(tbl, extensions = "Buttons", escape = FALSE,
              options = list(scrollX = TRUE, pageLength = 15, dom = "Bfrtip",
                             buttons = list(list(extend = "copy"), list(extend = "csv"), list(extend = "excel"))),
              rownames = FALSE)
  })
  
  filtered_structure <- reactive({
    if (identical(input$dataset_filter, "All")) structure_df else structure_df |> dplyr::filter(.data$dataset == input$dataset_filter)
  })
  
  output$structure_table <- renderDT({
    datatable(sanitize_for_display(filtered_structure()), options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
  })
  
  observeEvent(input$open_export_modal, {
    showModal(modalDialog(
      title = "Export to Excel",
      textInput("export_title", "Table Title", value = "", placeholder = "Example: Table 25. Distribution..."),
      p(class = "text-sm text-slate-500", "Enter the title for the exported Excel file."),
      footer = tagList(modalButton("Cancel"), downloadButton("download_xlsx", "Download Excel", class = "btn btn-primary")),
      easyClose = TRUE
    ))
  })
  
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("cbms_table_builder_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) { export_ai_table(current_preview_table(), file, title = input$export_title %||% "") }
  )
  
  observeEvent(input$save_spec, {
    showModal(modalDialog(title = "Save Specification", textInput("spec_title", "Title"),
                          footer = tagList(modalButton("Cancel"), actionButton("confirm_save", "Save"))))
  })
  
  observeEvent(input$confirm_save, {
    title <- input$spec_title
    if (nzchar(title)) {
      if (!saved_specs$loaded) load_specs_from_file()
      if (title %in% names(saved_specs$specs)) {
        showModal(modalDialog(title = "Overwrite?", paste("'", title, "' already exists. Overwrite?"),
                              footer = tagList(modalButton("Cancel"), actionButton("confirm_overwrite", "Overwrite"))))
      } else {
        current_spec <- builder_spec(); current_spec$saved_at <- Sys.time()
        saved_specs$specs[[title]] <- current_spec; persist_specs(); removeModal()
        showNotification(paste("Saved:", title), type = "message")
      }
    } else { showNotification("Enter a title", type = "error") }
  })
  
  observeEvent(input$confirm_overwrite, {
    title <- input$spec_title; current_spec <- builder_spec()
    if (!saved_specs$loaded) load_specs_from_file()
    current_spec$saved_at <- Sys.time(); saved_specs$specs[[title]] <- current_spec
    persist_specs(); removeModal(); showNotification(paste("Overwritten:", title), type = "message")
  })
  
  observeEvent(input$save_manual_spec, {
    showModal(modalDialog(title = "Save Manual Specification", textInput("manual_spec_title", "Title"),
                          footer = tagList(modalButton("Cancel"), actionButton("confirm_save_manual", "Save"))))
  })
  
  observeEvent(input$confirm_save_manual, {
    title <- input$manual_spec_title
    if (nzchar(title)) {
      if (!saved_manual_specs$loaded) load_manual_specs_from_file()
      if (title %in% names(saved_manual_specs$specs)) {
        showModal(modalDialog(title = "Overwrite?", paste("'", title, "' already exists. Overwrite?"),
                              footer = tagList(modalButton("Cancel"), actionButton("confirm_overwrite_manual", "Overwrite"))))
      } else {
        new_spec <- list(code = input$manual_code %||% "", saved_at = Sys.time())
        saved_manual_specs$specs[[title]] <- new_spec; persist_manual_specs(); removeModal()
        showNotification(paste("Saved:", title), type = "message")
      }
    } else { showNotification("Enter a title", type = "error") }
  })
  
  observeEvent(input$confirm_overwrite_manual, {
    title <- input$manual_spec_title
    new_spec <- list(code = input$manual_code %||% "", saved_at = Sys.time())
    if (!saved_manual_specs$loaded) load_manual_specs_from_file()
    saved_manual_specs$specs[[title]] <- new_spec; persist_manual_specs(); removeModal()
    showNotification(paste("Overwritten:", title), type = "message")
  })
  
  output$saved_manual_specs_table <- renderDT({
    req(saved_manual_specs$loaded)
    specs <- saved_manual_specs$specs
    if (length(specs) == 0) return(datatable(data.frame(Message = "No saved manual code yet."), options = list(dom = "t"), rownames = FALSE))
    df <- data.frame(Title = names(specs), Code = sapply(specs, function(x) { c <- x$code; if (nchar(c) > 80) paste0(substr(c,1,80),"...") else c }), stringsAsFactors = FALSE)
    actions <- vapply(df$Title, function(title) {
      as.character(tagList(
        actionButton(paste0("view_manual_", gsub("[^A-Za-z0-9]","_",title)), "View", class = "btn-sm btn-primary btn-block",
                     onclick = sprintf('Shiny.setInputValue("view_manual_spec","%s",{priority:"event"});', title)),
        " ", actionButton(paste0("delete_manual_", gsub("[^A-Za-z0-9]","_",title)), "Delete", class = "btn-sm btn-danger btn-block",
                          onclick = sprintf('Shiny.setInputValue("delete_manual_spec","%s",{priority:"event"});', title))
      ))
    }, character(1))
    df$Actions <- actions
    datatable(df, escape = FALSE, options = list(pageLength = 10, scrollX = TRUE,
                                                 columnDefs = list(list(targets = 0:1, render = JS("function(d,t,r,m){return t==='display'&&d!==null&&d.length>80?d.substr(0,80)+'...':d;}")),
                                                                   list(targets = 2, orderable = FALSE, searchable = FALSE))), rownames = FALSE)
  })
  
  observeEvent(input$view_manual_spec, {
    title <- input$view_manual_spec; spec <- saved_manual_specs$specs[[title]]
    if (is.null(spec)) { showNotification("Not found.", type = "error"); return() }
    update_ui_from_manual_spec(spec); showNotification(paste("Loaded:", title), type = "message")
  })
  
  observeEvent(input$delete_manual_spec, {
    title <- input$delete_manual_spec
    if (title %in% names(saved_manual_specs$specs)) { saved_manual_specs$specs[[title]] <- NULL; persist_manual_specs(); showNotification(paste("Deleted:", title), type = "warning") }
    else { showNotification("Not found.", type = "error") }
  })
  
  output$saved_specs_table <- renderDT({
    req(saved_specs$loaded)
    specs <- saved_specs$specs
    if (length(specs) == 0) return(datatable(data.frame(Message = "No saved specifications yet."), options = list(dom = "t"), rownames = FALSE))
    df <- data.frame(Title = names(specs), Dataset = sapply(specs, `[[`, "dataset"),
                     JoinDataset = sapply(specs, function(x) if (isTRUE(x$join$enabled)) x$join$join_dataset else ""),
                     Join = sapply(specs, function(x) ifelse(isTRUE(x$join$enabled),"Yes","No")),
                     Rows = sapply(specs, function(x) { r <- paste(x$row_fields, collapse=", "); if (nchar(r)>50) paste0(substr(r,1,47),"...") else r }),
                     Columns = sapply(specs, function(x) { c <- paste(x$column_fields, collapse=", "); if (nchar(c)>50) paste0(substr(c,1,47),"...") else c }),
                     Filters = sapply(specs, function(x) { f <- x$filters; if (is.null(f)||length(f)==0||!nzchar(f)) "" else f }),
                     stringsAsFactors = FALSE)
    actions <- sapply(df$Title, function(title) {
      as.character(tagList(
        actionButton(paste0("view_", gsub("[^A-Za-z0-9]","_",title)), "View", class = "btn-sm btn-primary btn-block",
                     onclick = sprintf('Shiny.setInputValue("view_spec","%s",{priority:"event"});', title)),
        " ", actionButton(paste0("delete_", gsub("[^A-Za-z0-9]","_",title)), "Delete", class = "btn-sm btn-danger btn-block",
                          onclick = sprintf('Shiny.setInputValue("delete_spec","%s",{priority:"event"});', title))
      ))
    })
    df$Actions <- actions
    datatable(df, escape = FALSE, options = list(pageLength = 10, scrollX = TRUE,
                                                 columnDefs = list(list(targets = c(1,2,3,4,5), render = JS("function(d,t,r,m){return t==='display'&&d!==null&&d.length>100?d.substr(0,100)+'...':d;}")))), rownames = FALSE)
  })
  
  observeEvent(input$view_spec, {
    title <- input$view_spec; spec <- saved_specs$specs[[title]]
    if (is.null(spec)) { showNotification("Not found.", type = "error"); return() }
    updateCheckboxInput(session, "use_manual_code", value = FALSE); update_ui_from_spec(spec)
    later::later(function() { session$sendCustomMessage("shinyActionButtonValue", list(id = "run_builder", value = as.numeric(Sys.time()))) }, delay = 2.5)
    showNotification(paste("Loaded:", title), type = "message")
  })
  
  observeEvent(input$delete_spec, {
    title <- input$delete_spec
    if (title %in% names(saved_specs$specs)) { saved_specs$specs[[title]] <- NULL; persist_specs(); showNotification(paste("Deleted:", title), type = "warning") }
    else { showNotification("Not found.", type = "error") }
  })
}

shinyApp(ui, server)