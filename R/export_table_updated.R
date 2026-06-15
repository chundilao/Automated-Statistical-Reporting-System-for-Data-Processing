export_ai_table <- function(tbl, file, title = NULL, sheet_name = "Table 1") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package `openxlsx` is required for Excel export.")
  }
  if (is.null(tbl) || !is.data.frame(tbl)) {
    stop("`tbl` must be a data.frame.")
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)

  # ---- detect column groups ----
  all_cols <- names(tbl)
  row_vars <- all_cols[!(grepl("^(Frequency_|Percent_)", all_cols) | all_cols %in% "TOTAL")]
  freq_cols <- grep("^Frequency_", all_cols, value = TRUE)
  pct_cols  <- grep("^Percent_", all_cols, value = TRUE)

  # TOTAL should appear after row vars in exported sheet as well
  ordered_cols <- c(row_vars, intersect("TOTAL", all_cols), freq_cols, pct_cols)
  ordered_cols <- unique(ordered_cols[ordered_cols %in% all_cols])
  tbl <- tbl[, ordered_cols, drop = FALSE]
  all_cols <- names(tbl)

  header_labels <- attr(tbl, "header_labels")
  if (is.null(header_labels)) header_labels <- list()

  # ---- helper label formatters ----
  get_subheader_label <- function(col_name) {
    lbl <- header_labels[[col_name]]
    if (is.null(lbl) || !nzchar(lbl)) {
      key <- sub("^[^_]+_", "", col_name)
      return(key)
    }
    parts <- strsplit(as.character(lbl), "_", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      paste(parts[-1], collapse = "_")
    } else {
      lbl
    }
  }

  get_top_header <- function(col_name) {
    if (col_name %in% row_vars) return(col_name)
    if (identical(col_name, "TOTAL")) return("Both Sexes")
    if (startsWith(col_name, "Frequency_")) return("Frequency")
    if (startsWith(col_name, "Percent_")) return("Percent")
    col_name
  }

  get_second_header <- function(col_name) {
    if (col_name %in% row_vars) return("")
    if (identical(col_name, "TOTAL")) return("")
    if (startsWith(col_name, "Frequency_") || startsWith(col_name, "Percent_")) {
      return(get_subheader_label(col_name))
    }
    ""
  }

  # ---- optional title ----
  export_title <- title %||% attr(tbl, "export_title")
  start_row <- 1
  if (!is.null(export_title) && nzchar(export_title)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = export_title, startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet = sheet_name, cols = 1:length(all_cols), rows = 1)
    start_row <- 3
  }

  # ---- build two-row header ----
  top_header <- vapply(all_cols, get_top_header, character(1))
  second_header <- vapply(all_cols, get_second_header, character(1))

  openxlsx::writeData(wb, sheet = sheet_name, x = as.list(top_header), startRow = start_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet = sheet_name, x = as.list(second_header), startRow = start_row + 1, startCol = 1, colNames = FALSE)

  # Merge row-var and TOTAL headers vertically
  for (i in seq_along(all_cols)) {
    if (all_cols[[i]] %in% row_vars || identical(all_cols[[i]], "TOTAL")) {
      openxlsx::mergeCells(wb, sheet = sheet_name, cols = i, rows = c(start_row, start_row + 1))
    }
  }

  # Merge Frequency group horizontally
  if (length(freq_cols) > 0) {
    idx <- match(freq_cols, all_cols)
    openxlsx::mergeCells(wb, sheet = sheet_name, cols = min(idx):max(idx), rows = start_row)
  }

  # Merge Percent group horizontally
  if (length(pct_cols) > 0) {
    idx <- match(pct_cols, all_cols)
    openxlsx::mergeCells(wb, sheet = sheet_name, cols = min(idx):max(idx), rows = start_row)
  }

  # ---- write data ----
  openxlsx::writeData(wb, sheet = sheet_name, x = tbl, startRow = start_row + 2, startCol = 1, colNames = FALSE, rowNames = FALSE)

  # ---- styles ----
  title_style <- openxlsx::createStyle(fontSize = 13, textDecoration = "bold", halign = "left", valign = "center")
  header_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center", valign = "center",
    border = "TopBottomLeftRight", wrapText = TRUE
  )
  subheader_style <- openxlsx::createStyle(
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", wrapText = TRUE
  )
  text_style <- openxlsx::createStyle(halign = "left", valign = "center")
  integer_style <- openxlsx::createStyle(halign = "right", valign = "center", numFmt = "#,##0")
  percent_style <- openxlsx::createStyle(halign = "right", valign = "center", numFmt = "0.00")

  if (!is.null(export_title) && nzchar(export_title)) {
    openxlsx::addStyle(wb, sheet = sheet_name, style = title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, sheet = sheet_name, rows = 1, heights = 22)
  }

  openxlsx::addStyle(wb, sheet = sheet_name, style = header_style, rows = start_row, cols = 1:length(all_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name, style = subheader_style, rows = start_row + 1, cols = 1:length(all_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(wb, sheet = sheet_name, rows = c(start_row, start_row + 1), heights = c(22, 24))

  data_row_start <- start_row + 2
  data_row_end <- data_row_start + nrow(tbl) - 1
  if (nrow(tbl) > 0) {
    text_cols <- which(all_cols %in% row_vars)
    int_cols <- which(all_cols %in% c("TOTAL", freq_cols))
    pct_num_cols <- which(all_cols %in% pct_cols)

    if (length(text_cols) > 0) {
      openxlsx::addStyle(wb, sheet = sheet_name, style = text_style, rows = data_row_start:data_row_end, cols = text_cols, gridExpand = TRUE, stack = TRUE)
    }
    if (length(int_cols) > 0) {
      openxlsx::addStyle(wb, sheet = sheet_name, style = integer_style, rows = data_row_start:data_row_end, cols = int_cols, gridExpand = TRUE, stack = TRUE)
    }
    if (length(pct_num_cols) > 0) {
      openxlsx::addStyle(wb, sheet = sheet_name, style = percent_style, rows = data_row_start:data_row_end, cols = pct_num_cols, gridExpand = TRUE, stack = TRUE)
    }
  }

  # Borders around data region
  border_style <- openxlsx::createStyle(border = "TopBottomLeftRight")
  openxlsx::addStyle(
    wb, sheet = sheet_name, style = border_style,
    rows = start_row:(start_row + 1 + nrow(tbl)), cols = 1:length(all_cols),
    gridExpand = TRUE, stack = TRUE
  )

  # Column widths
  widths <- vapply(all_cols, function(nm) {
    if (nm %in% row_vars) return(22)
    if (nm == "TOTAL") return(14)
    12
  }, numeric(1))
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:length(all_cols), widths = widths)
  openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = start_row + 2, firstActiveCol = 1)

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x
}
