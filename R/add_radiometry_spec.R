#' Add a calibration spec to an existing geometry spec
#'
#' @description
#' Add a radiometric calibration specification to an existing geometry
#' specification in a hemispherical camera registry.
#'
#' @param registry `hemispherical_camera_registry` object created with
#'   [new_registry()] and containing the geometry specification identified by
#'   `geometry_id`.
#' @param geometry_id character vector of length one. Identifier of the geometry
#'   specification to which the radiometry spec will be attached.
#' @param id character vector of length one. Identifier of the radiometry spec.
#' @param type character vector of length one. Type of radiometric correction.
#'   Only "vignetting" is currently supported.
#' @param method character vector of length one. Method used to acquire the
#'   primary data needed for radiometric calibration. Supported methods are
#'   "simple" and "photometric_sphere".
#' @param model character vector of length one. Model used to develop a
#'   radiometric correction. Only "polynomial" is currently supported via
#'   [rcaiman::correct_vignetting].
#' @param parameters numeric vector. Model parameters defining the radiometric
#'   correction, following the conventions of the selected `model`.
#'
#' @inheritParams add_geometry_spec
#'
#' @return
#' A `hemispherical_camera_registry` object with the radiometry specification
#' appended to the selected geometry specification.
#'
#' @export
#'
add_radiometry_spec <- function(
    registry,
    geometry_id,
    id,
    type = "vignetting",
    method,
    model,
    parameters,
    date,
    notes = NULL,
    contact_information = NULL
) {

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

  .assert_choice(type, "vignetting")
  .assert_choice(method, c("simple", "photometric_sphere"))
  .assert_choice(model, "polynomial")
  .check_vector(parameters, "numeric")
  .check_vector(date, "date", 1)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  radiometry_spec <- list(
    id = id,
    type = type,
    method = method,
    model = model,
    parameters = parameters,
    date = date,
    notes = notes,
    contact_information = contact_information
  )

  registry[[geometry_id]]$radiometry[[id]] <- radiometry_spec

  registry
}
