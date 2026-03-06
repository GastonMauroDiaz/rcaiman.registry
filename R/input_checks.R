.assert_id <- function(id, name = deparse(substitute(id))) {
  if (!grepl("^[a-z0-9_]+$", id)) {
    stop(
      sprintf("`%s`  must use snake_case and contain only lowercase letters, numbers, and underscores.", name)
    )
  }
}

.check_registry_entry <- function(registry_entry) {
  if (!inherits(registry_entry, "hs_registry_entry")) {
    stop(
      "`registry_entry` must be an object of class 'hs_registry_entry'.",
      call. = FALSE
    )
  }
}

.check_geometry_spec <- function(x) {
  if (!inherits(x, "geometry_spec")) {
    stop(
      "The geometry specification must be an object of class 'geometry_spec'.",
      call. = FALSE
    )
  }
  if (is.null(x$radiometry) || !is.list(x$radiometry)) {
    stop(
      "Radiometry container missing in geometry '",
      x$id,
      "'"
    )
  }
}

.check_embedded_metadata_sig <- function(x) {
  if (!inherits(x, "embedded_metadata_sig")) {
    stop(
      "The embedded metadata signature must be an object of class 'embedded_metadata_sig'.",
      call. = FALSE
    )
  }
  if (is.null(x$geometry) || !is.list(x$geometry)) {
    stop(
      "Geometry container missing in embedded metadata '",
      x$id,
      "'"
    )
  }
}

.check_file_sig <- function(x) {
  if (!inherits(x, "file_sig")) {
    stop(
      "The file signature must be an object of class 'file_sig'.",
      call. = FALSE
    )
  }
}

.check_flat_field_model <- function(model) {
  valid_flat_field_generators <- c(
    "flat_field_free_form_polynomial",
    "flat_field_lang2010",
    "flat_field_simple_polynomial",
    "flat_field_trend_surface"
  )

  .check_vector(model, "character", 1)

  if (!model %in% valid_flat_field_generators)
    stop("`model` is not a supported flat-field model.\n",
         "Supported models are: ",
         paste0(valid_flat_field_generators, collapse = ", "),
         call. = FALSE)

  if (!exists(model, envir = asNamespace("rcaiman"), mode = "function"))
    stop("`model` is not available in the installed rcaiman version.",
         call. = FALSE)

  invisible(TRUE)
}

