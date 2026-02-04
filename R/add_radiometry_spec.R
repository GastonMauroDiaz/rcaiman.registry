#' Add a radiometry spec to an existing geometry spec
#'
#' @description
#' Add a radiometry specification to an existing geometry
#' specification in a hemispherical camera registry.
#'
#' @details
#' Radiometry specifications intentionally separate *parameter values* from
#' *model shape*. The `scheme` and `model_type` arguments provide the semantic
#' context required by `rcaiman` to interpret those parameters and apply the
#' corresponding correction.
#'
#' This design is deliberate. The effective mathematical model applied to images
#' is defined by the functions that consume the registry (e.g.
#' [rcaiman::correct_vignetting()]), not by the registry itself. The registry
#' stores parameter values together with the minimal semantic information
#' required for their correct interpretation.
#'
#' @param registry `hemispherical_camera_registry` object created with
#'   [new_registry()] and containing the geometry specification identified by
#'   `geometry_id`.
#' @param geometry_id character vector of length one. Identifier of the geometry
#'   specification to which the radiometry spec will be attached.
#' @param id character vector of length one. Identifier of the radiometry spec.
#'   Must use snake_case (lowercase letters, numbers, and underscores only)
#' @param type character vector of length one. Type of radiometric correction.
#'   Only "vignetting" is currently supported.
#' @param scheme character vector of length one. Calibration scheme defining
#'   the modeling assumptions used by the core for radiometric correction.
#'   The selected scheme determines the effective form of the model and the
#'   constraints (if any) applied to its parameters. Recognized schemes
#'   currently implemented in `rcaiman`: `"simple"` and `"free_form"`.
#' @param model_type character vector of length one. Model type used to develop a
#'   radiometric correction. Only `"polynomial"` is currently implemented in
#'   `rcaiman`.
#' @param parameters A named list of model parameters. Model parameters defining
#'   the radiometric correction, following the conventions of the selected
#'   `model_type`. List names must be coercible to numeric values representing
#'   f-numbers.
#'
#' @inheritParams add_geometry_spec
#'
#' @return
#' A `hemispherical_camera_registry` object with the radiometry specification
#' appended to the selected geometry specification.
#'
#' @export
#'
#' @examples
#' foo <- new_registry(
#'   "Nikon_D610.Nikkor_8mm.CIEFAP",
#'   body = "D610",
#'   body_manufacturer = "NIKON CORP",
#'   body_serial = "9023728",
#'   lens = "AF-S FISHEYE NIKKOR 8-15mm 1:3.5-4.5E ED",
#'   lens_manufacturer = "NIKON CORP",
#'   lens_serial = "210020",
#'   institution = "CIEFAP"
#' )
#'
#' foo <- add_geometry_spec(
#'   foo,
#'   id = "simple_method",
#'   lens_coef = signif(c(1306,24.8,-56.2)/1894,3),
#'   zenith_colrow = c(1500, 997),
#'   horizon_radius = 947,
#'   max_zenith_angle = 92.8,
#'   dim = c(3040, 2014),
#'   firmware_version = "1.01",
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' foo <- add_radiometry_spec(
#'   foo,
#'   geometry_id = "simple_method",
#'   id = "simple_method",
#'   type = "vignetting",
#'   scheme = "simple",
#'   model_type = "polynomial",
#'   parameters = list("3.5" = c(0.0302, 0.320, 0.0908)),
#'   firmware_version = "1.01",
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
add_radiometry_spec <- function(
    registry,
    geometry_id,
    id,
    type = "vignetting",
    scheme,
    model_type,
    parameters,
    date = NULL,
    firmware_version = NULL,
    notes = NULL,
    contact_information = NULL
) {

  if (is.null(date)) {
    date <- Sys.Date()
  }

  if (!inherits(registry, "hemispherical_camera_registry")) {
    stop(
      "`registry` must be an object of class 'hemispherical_camera_registry'.",
      call. = FALSE
    )
  }

  .check_vector(geometry_id, "character", 1)
  if (is.null(registry[[geometry_id]])) {
    stop(
      sprintf("Geometry spec with id '%s' does not exist in this registry.", geometry_id),
      call. = FALSE
    )
  }

  geometry <- registry[[geometry_id]]
  if (is.null(geometry$radiometry)) {
    geometry$radiometry <- list()
  }

  if (!is.null(geometry$radiometry[[id]])) {
    stop(
      sprintf(
        "Radiometry spec with id '%s' already exists for geometry '%s'.",
        id, geometry_id
      ),
      call. = FALSE
    )
  }

  # .assert_choice(type, "vignetting")
  # .assert_choice(scheme, c("simple", "photometric_sphere"))
  # .assert_choice(model_type, "polynomial")
  .check_vector(type, "character", 1)
  .check_vector(type, "character", 1)
  if (!is.list(parameters)) {
    stop("`parameters` must be a list.")
  }
  nms <- names(parameters)
  if (is.null(nms) || any(nms == "")) {
    stop("`parameters` must be a named list with names representing aperture values.")
  }
  aperture_num <- suppressWarnings(as.numeric(nms))
  if (any(is.na(aperture_num))) {
    stop("All `parameters` names must be coercible to numeric aperture values (f-numbers).")
  }
  if (any(!is.finite(aperture_num)) || any(aperture_num <= 0)) {
    stop("Aperture values in `parameters` must be positive and finite.")
  }
  if (any(duplicated(aperture_num))) {
    stop("Duplicated aperture values detected in `parameters`.")
  }
  ok <- vapply(
    parameters,
    function(x) is.numeric(x) && length(x) >= 1,
    logical(1)
  )
  if (!all(ok)) {
    stop("Each element of `parameters` must be a numeric vector of length >= 1.")
  }
  .check_vector(date, "date", 1)
  .check_vector(firmware_version, "character", 1, allow_null = TRUE)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  radiometry_spec <- list(
    id = id,
    type = type,
    scheme = scheme,
    model_type = model_type,
    parameters = parameters,
    date = date,
    firmware_version = firmware_version,
    notes = notes,
    contact_information = contact_information
  )

  registry[[geometry_id]]$radiometry[[id]] <- radiometry_spec

  registry
}
