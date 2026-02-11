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
#'   used in \pkg{rcaiman}.
#' @param horizon_radius integer-like numeric vector of length one. Radius of the
#'   hemispherical image in pixel units, measured from the optical center
#'   (`zenith_colrow`) to the image location corresponding to a zenith angle of
#'   90 deg (i.e. the horizon), and following the conventions used in \pkg{rcaiman}
#'   (i.e., `2 * horizon_radius` must be an even integer, see
#'   [rcaiman::zenith_image]).
#' @param is_horizon_circle_clipped logical vector of length one. Indicates
#'   whether the horizon circle (90° zenith angle), obtained via model
#'   extrapolation when necesary, is clipped by the image matrix. If `FALSE`,
#'   the geometry specification asserts that the complete horizon circle is
#'   fully included within the image extent, and this condition may be validated
#'   geometrically. If `TRUE`, horizon clipping is expected and no such
#'   validation is performed.
#' @param max_zenith_angle optional numeric vector of length one. Maximum zenith
#'   angle (deg) that the system is capable of observing.
#' @param dim optional integer-like numeric vector of length two. Width and height in
#'   pixels of the raster from which `zenith_colrow` was derived.
#' @param firmware_version optional character vector of length one.
#'   Firmware version of the image acquisition system at the time the
#'   calibration images were acquired. This information is stored for
#'   documentation and traceability purposes.
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
#'   horizon_radius = 946,
#'   is_horizon_circle_clipped = FALSE,
#'   max_zenith_angle = 92.8,
#'   dim = c(3040, 2014),
#'   firmware_version = "1.01",
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
add_geometry_spec <- function(
    registry,
    id,
    date = NULL,
    lens_coef,
    zenith_colrow,
    horizon_radius,
    is_horizon_circle_clipped,
    max_zenith_angle = NULL,
    dim = NULL,
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

  .check_vector(id, "character", 1)
  if (!is.null(registry[[id]])) {
    stop(
      sprintf("A geometry spec with id '%s' already exists in this registry.", id),
      call. = FALSE
    )
  }
  .assert_id(id)
  .check_vector(date, "date", 1)
  .check_vector(lens_coef, "numeric")
  .check_vector(zenith_colrow, "numeric", length = 2, sign = "positive")
  .check_vector(horizon_radius, "even_integerish", length = 1)
  .check_vector(is_horizon_circle_clipped, "logical")
  .check_vector(max_zenith_angle, "numeric", length = 1, allow_null = TRUE, sign = "positive")
  .check_vector(dim, "integerish", length = 2, allow_null = TRUE)
  .check_vector(firmware_version, "character", 1, allow_null = TRUE)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  geometry_spec <- list(
    id = id,
    lens_coef = lens_coef,
    zenith_colrow = zenith_colrow,
    horizon_radius = horizon_radius,
    is_horizon_circle_clipped = is_horizon_circle_clipped,
    max_zenith_angle = max_zenith_angle,
    dim = dim,
    date = date,
    firmware_version = firmware_version,
    notes = notes,
    contact_information = contact_information,
    radiometry = list()
  )

  .validate_geometry_spec(geometry_spec)

  registry[[id]] <- geometry_spec
  registry
}
