`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x

resolve_join_field_names <- function(base_names, join_keys, join_fields) {
  join_fields <- unique(join_fields %||% character(0))
  out <- setNames(join_fields, join_fields)
  non_key_fields <- setdiff(join_fields, join_keys %||% character(0))

  for (nm in non_key_fields) {
    if (nm %in% base_names) {
      out[[nm]] <- paste0(nm, "_join")
    }
  }

  out
}

build_joined_dataset <- function(data, join_spec = NULL) {
  if (is.null(join_spec) || !isTRUE(join_spec$enabled)) {
    return(list(
      data = data[[join_spec$base_dataset %||% names(data)[[1]]]],
      field_map = stats::setNames(names(data[[join_spec$base_dataset %||% names(data)[[1]]]]), names(data[[join_spec$base_dataset %||% names(data)[[1]]]]))
    ))
  }

  base_dataset <- join_spec$base_dataset
  join_dataset <- join_spec$join_dataset
  join_keys <- unique(join_spec$join_keys %||% character(0))
  join_fields <- unique(join_spec$join_fields %||% character(0))
  join_type <- join_spec$join_type %||% "left"

  if (!base_dataset %in% names(data)) stop(sprintf("Base dataset `%s` not found.", base_dataset))
  if (!join_dataset %in% names(data)) stop(sprintf("Join dataset `%s` not found.", join_dataset))

  x <- data[[base_dataset]]
  y_raw <- data[[join_dataset]]

  if (length(join_keys) == 0) stop("At least one join key is required.")
  missing_x <- setdiff(join_keys, names(x))
  missing_y <- setdiff(join_keys, names(y_raw))
  if (length(missing_x) > 0 || length(missing_y) > 0) {
    stop(sprintf(
      "Join key(s) missing. Base missing: %s | Join missing: %s",
      paste(missing_x, collapse = ", "),
      paste(missing_y, collapse = ", ")
    ))
  }

  join_fields <- setdiff(join_fields, join_keys)
  missing_join_fields <- setdiff(join_fields, names(y_raw))
  if (length(missing_join_fields) > 0) {
    stop(sprintf("Join field(s) not found in `%s`: %s", join_dataset, paste(missing_join_fields, collapse = ", ")))
  }

  rename_map <- resolve_join_field_names(names(x), join_keys, join_fields)

  y <- y_raw[, unique(c(join_keys, join_fields)), drop = FALSE]
  if (length(join_fields) > 0) {
    old_names <- join_fields
    new_names <- unname(rename_map[join_fields])
    to_rename <- old_names[new_names != old_names]
    if (length(to_rename) > 0) {
      rename_expr <- stats::setNames(rlang::syms(to_rename), new_names[new_names != old_names])
      y <- dplyr::rename(y, !!!rename_expr)
    }
  }

  joined <- switch(
    join_type,
    left = dplyr::left_join(x, y, by = join_keys),
    inner = dplyr::inner_join(x, y, by = join_keys),
    full = dplyr::full_join(x, y, by = join_keys),
    right = dplyr::right_join(x, y, by = join_keys),
    dplyr::left_join(x, y, by = join_keys)
  )

  field_map <- stats::setNames(names(joined), names(joined))
  list(data = joined, field_map = field_map, rename_map = rename_map)
}
