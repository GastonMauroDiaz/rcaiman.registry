#' Add a geometry spec to an existing registry
#'
#' Add a geometry specification to an existing hemispherical
#' camera registry.
#'
#' @param registry `hemispherical_camera_registry` object created with
#'   [new_registry()].
#' @param id character vector of length one. Identifier of the geometry spec.
#'   Must use snake_case (lowercase letters, numbers, and underscores only)
#' @param date optional [`Date`] object. Date when the specification was added
#'   to the registry. If `NULL`, the current system date is used automatically.
#' @param lens_coef numeric vector. Lens projection coefficients describing the
#'   relationship between image radius and zenith angle. These coefficients
#'   typically originate from [rcaiman::calibrate_lens()] or compatible
#'   procedures following the same conventions. The coefficients must pass
#'   [rcaiman::test_lens_coef()].
#' @param zenith_colrow numeric vector of length 2. Column and row coordinates
#'   of the optical center (zenith) in pixel units following the conventions
#'   used in `rcaiman`.
#' @param horizon_radius integer-like numeric vector of length one. Radius of the
#'   hemispherical image in pixel units, measured from the optical center
#'   (`zenith_colrow`) to the image location corresponding to a zenith angle of
#'   90 deg (i.e. the horizon), and following the conventions used in `rcaiman`
#'   (i.e., `2 * horizon_radius` must be an even integer, see
#'   [rcaiman::zenith_image]).
#' @param max_zenith_angle optional numeric vector of length one. Maximum zenith
#'   angle (deg) that the system is capable of observing.
#' @param dim integer-like numeric vector of length two. Width and height in
#'   pixels of the raster from which `zenith_colrow` was derived.
#' @param notes optional character vector of length one. Free-form notes
#'   providing context, scope, assumptions, or references relevant to this
#'   specification (e.g. publications).
#' @param contact_information optional character vector of length one. Contact
#'   information for the specification maintainer or responsible person.
#'
#' @return
#' A `hemispherical_camera_registry` object with the geometry specification
#' appended.
#'
#' @export
#'
add_geometry_spec <- function(
    registry,
    id,
    date = NULL,
    lens_coef,
    zenith_colrow,
    horizon_radius,
    max_zenith_angle = NULL,
    dim = NULL,
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

  if (!is.null(registry[[id]])) {
    stop(
      sprintf("A geometry spec with id '%s' already exists in this registry.", id),
      call. = FALSE
    )
  }

  .check_vector(id, "character", 1)
  .assert_id(id)
  .check_vector(date, "date", 1)
  .check_vector(lens_coef, "numeric")
  .check_vector(zenith_colrow, "numeric", length = 2)
  .check_vector(horizon_radius, "integerish", length = 1)
  .check_vector(max_zenith_angle, "numeric", length = 1, allow_null = TRUE)
  .check_vector(dim, "integerish", length = 2, allow_null = TRUE)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  geometry_spec <- list(
    id = id,
    lens_coef = lens_coef,
    zenith_colrow = zenith_colrow,
    horizon_radius = horizon_radius,
    max_zenith_angle = max_zenith_angle,
    dim = dim,
    date = date,
    notes = notes,
    contact_information = contact_information,
    radiometry = list()
  )

  registry[[id]] <- geometry_spec
  registry
}
