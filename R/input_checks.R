.assert_id <- function(id, name = deparse(substitute(id))) {
  if (!grepl("^[a-z0-9_]+$", id)) {
    stop(
      sprintf(
        "`%s`  must use snake_case and contain only lowercase letters, numbers, and underscores.",
        name
      )
    )
  }
}

.check_registry_entry <- function(registry_entry) {
  if (!inherits(registry_entry, "hs_registry_entry")) {
    stop("`registry_entry` must be an object of class 'hs_registry_entry'.",
         call. = FALSE)
  }
}

.check_geometry_spec <- function(x) {
  if (!inherits(x, "geometry_spec")) {
    stop("The geometry specification must be an object of class 'geometry_spec'.",
         call. = FALSE)
  }
  if (is.null(x$radiometry) || !is.list(x$radiometry)) {
    stop("Radiometry container missing in geometry '", x$id, "'")
  }

  rcaiman::.check_internal_geometry(x)
  .check_vector(x$id, "character", 1)
  .assert_id(x$id)
  .check_vector(x$date, "date", 1)
  .check_vector(x$notes, "character", 1, allow_null = TRUE)
  .check_vector(x$contact_information, "character", 1, allow_null = TRUE)
  observed <- x$max_observed_off_axis_angle
  .check_vector(observed, "numeric", 1, allow_null = TRUE,
                name = "max_observed_off_axis_angle")
  if (!is.null(observed) && (observed < 0 || observed > 180)) {
    stop("`max_observed_off_axis_angle` must lie within [0, 180] degrees.",
         call. = FALSE)
  }

  invisible(TRUE)
}

.check_points_coord <- function(x, name = deparse(substitute(x))) {
  .check_columns(x, required_columns = c("x", "y_down"))
  y_down <- x$y_down
  x <- x$x
  .check_vector(x, "numeric")
  .check_vector(y_down, "numeric")
}

.check_signature <- function(x) {
  if (!inherits(x, "signature")) {
    stop(
      "The signature must be an object of class 'signature'.",
      call. = FALSE
    )
  }
  if (is.null(x$bands) || !is.list(x$bands)) {
    stop(
      "Band container missing in signature '",
      x$id,
      "'"
    )
  }
}

.check_band_spec <- function(x) {
  if (!inherits(x, "band_spec")) {
    stop("The band specification must be an object of class 'band_spec'.",
         call. = FALSE)
  }
  .check_vector(x$id, "character", 1)
  .assert_id(x$id)
  .check_vector(x$date, "date", 1)
  .check_vector(x$notes, "character", 1, allow_null = TRUE)
  .check_vector(x$contact_information, "character", 1, allow_null = TRUE)
  if (is.null(x$geometry) || !is.list(x$geometry)) {
    stop("Geometry container missing in band specification '", x$id, "'.",
         call. = FALSE)
  }
  if (!is.null(x$cfa_pattern)) .check_cfa_pattern(x$cfa_pattern)
  if (!is.null(x$spectral_mapping)) {
    if (is.null(x$cfa_pattern)) {
      stop("`spectral_mapping` requires `cfa_pattern`.", call. = FALSE)
    }
    .check_spectral_mapping(x$spectral_mapping, x$cfa_pattern)
  }
  invisible(TRUE)
}

.check_flat_field_model <- function(model) {
  valid_flat_field_generators <- c(
    "free_form_polynomial",
    "lang2010",
    "simple_polynomial",
    "predict_trend_surface"
  )

  .check_vector(model, "character", 1)

  if (!model %in% valid_flat_field_generators)
    stop("`model` is not a supported flat-field model.\n",
         "Supported models are: ",
         paste0(valid_flat_field_generators, collapse = ", "),
         call. = FALSE)

  evaluator <- if (model == "predict_trend_surface") model else "calc_relative_response"
  if (!exists(evaluator, envir = asNamespace("rcaiman"), mode = "function"))
    stop("`model` is not available in the installed rcaiman version.",
         call. = FALSE)

  invisible(TRUE)
}

.check_radiometry_parameters <- function(parameters, model) {
  if (model == "lang2010") {
    if (!is.null(parameters)) {
      stop("`parameters` must be NULL for lang2010; supply aperture when evaluating the model.",
           call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!is.list(parameters) || length(parameters) == 0) {
    stop("`parameters` must be a non-empty named list by aperture.", call. = FALSE)
  }
  nms <- names(parameters)
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    stop("`parameters` must have names representing aperture values.", call. = FALSE)
  }
  aperture <- suppressWarnings(as.numeric(nms))
  if (any(!is.finite(aperture)) || any(aperture < 0)) {
    stop("Aperture names must be finite, non-negative numeric values.", call. = FALSE)
  }
  if (anyDuplicated(aperture)) {
    stop("Duplicated aperture values detected in `parameters`.", call. = FALSE)
  }
  for (bands in parameters) {
    if (!is.list(bands) || !length(bands) || is.null(names(bands)) ||
        anyNA(names(bands)) || any(!nzchar(trimws(names(bands)))) ||
        anyDuplicated(names(bands))) {
      stop("Each aperture must contain a non-empty list with unique band names.", call. = FALSE)
    }
    for (value in bands) {
    if (model == "predict_trend_surface") {
      rcaiman::.check_trend_surface_parameters(value)
    } else {
      rcaiman::calc_relative_response(numeric(), model, value)
    }
  }
  }
  invisible(TRUE)
}
