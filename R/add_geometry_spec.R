.new_geometry_spec <- function(
    id,
    model,
    parameters,
    snapped_zenith_coord,
    horizon_radius,
    is_horizon_circle_clipped,
    max_zenith_angle,
    date,
    notes,
    contact_information
) {

  .assert_id(id)
  .check_geometry_model(model, parameters)
  .check_vector(date, "date", 1)
  .check_vector(
    snapped_zenith_coord,
    "integerish",
    length = 2,
    sign = "positive"
  )
  .check_vector(horizon_radius, "integerish", length = 1, sign = "positive")
  .check_vector(is_horizon_circle_clipped, "logical", length = 1)
  .check_vector(max_zenith_angle, "numeric", length = 1, allow_null = TRUE, sign = "positive")
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  x <- list(
    id = id,
    model = model,
    parameters = parameters,
    snapped_zenith_coord = snapped_zenith_coord,
    horizon_radius = horizon_radius,
    is_horizon_circle_clipped = is_horizon_circle_clipped,
    max_zenith_angle = max_zenith_angle,
    date = date,
    notes = notes,
    contact_information = contact_information,
    radiometry = list()
  )

  class(x) <- "geometry_spec"
  x
}

#' Add a geometry spec to an existing registry entry
#'
#' @description
#' Add a [geometry_spec] to a [hs_registry_entry] object.
#'
#' @param id character vector of length one. Identifier of the geometry spec.
#'   Must use snake_case (lowercase letters, numbers, and underscores only)
#' @param embedded_metadata_sig character vector of length one. Identifier of
#'   embedded metadata signature to which the geometry spec will be attached.
#' @param model character vector of length one. Geometry model implemented in
#'   \pkg{rcaiman}. Either `"radial_projection"` or
#'   `"anisotropic_projection"`.
#' @param parameters Model parameters. For `"radial_projection"`, a numeric
#'   vector of coefficients describing the relationship between image radius
#'   and zenith angle, validated with
#'   [rcaiman::validate_projection_function()]. For
#'   `"anisotropic_projection"`, a named list following
#'   the parameter contract documented in
#'   [rcaiman::add_directions_from_model()].
#' @param snapped_zenith_coord integer-like numeric vector of length two.
#'   Continuous image coordinates of the snapped optical center in the original
#'   image frame, in the order `c(x, y_down)`. The coordinates identify the
#'   center of the nearest 2 x 2 cell window.
#' @param horizon_radius integer-like numeric vector of length one. Radius of the
#'   hemispherical image in pixel units, measured from the optical center
#'   (`snapped_zenith_coord`) to the image location corresponding to a zenith
#'   angle of 90 deg (i.e. the horizon). For `"radial_projection"`, it follows
#'   the conventions used in \pkg{rcaiman}; `2 * horizon_radius` is the diameter
#'   supplied to [rcaiman::zenith_image()].
#' @param is_horizon_circle_clipped logical vector of length one. Indicates
#'   whether the horizon circle (90 deg zenith angle), obtained via model
#'   extrapolation when necesary, is clipped by the image matrix. If `FALSE`,
#'   the geometry specification asserts that the complete horizon circle is
#'   fully included within the image extent, and this condition may be validated
#'   geometrically. If `TRUE`, horizon clipping is expected and no such
#'   validation is performed.
#' @param max_zenith_angle optional numeric vector of length one. Maximum zenith
#'   angle (deg) that the system is capable of observing.
#' @param notes optional character vector of length one. Free-form notes
#'   providing context, scope, assumptions, or references relevant to this
#'   specification (e.g. publications).
#' @param contact_information optional character vector of length one. Contact
#'   information for the specification maintainer or responsible person.
#'
#' @inheritParams add_embedded_metadata_sig
#'
#' @return
#' A [hs_registry_entry] object with the [geometry_spec] object
#' appended the selected [embedded_metadata_sig].
#'
#' @export
#'
#' @examples
#' foo <- new_registry_entry(
#'   "Nikon_Coolpix5700.FCE9.CIEFAP",
#'   body = "E5700",
#'   body_manufacturer = "NIKON CORP",
#'   body_serial = "7053067",
#'   lens = "Zoom Nikkor ED 8.9-71.2mm 1:2.8-4.2",
#'   lens_manufacturer = "NIKON CORP",
#'   auxiliary_lens = "Fisheye Converter FC-E9 0.2x",
#'   auxiliary_lens_manufacturer = "NIKON CORP",
#'   institution = "CIEFAP"
#' )
#'
#' path <- system.file("external/nikon_coolpix5700_fce9_ciefap_snippet.R",
#'                      package = "rcaiman.registry")
#' source(path)
#'
#' foo <- add_embedded_metadata_sig(
#'   foo,
#'   id = "exif_raw",
#'   dim = c(1288, 962),
#'   validated_with_libraw = "0.21.1",
#'   validated_with_exiftool = "13.59",
#'   rules = metadata_rules
#' )
#'
#' foo <- add_geometry_spec(
#'   foo,
#'   embedded_metadata_sig = "exif_raw",
#'   id = "simple_method",
#'   model = "radial_projection",
#'   parameters = c(0.6380,  0.0307, -0.0200),
#'   snapped_zenith_coord = c(645, 494),
#'   horizon_radius = 378,
#'   is_horizon_circle_clipped = FALSE,
#'   max_zenith_angle = 94.8,
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' foo
#' summary(foo)
add_geometry_spec <- function(
    registry_entry,
    embedded_metadata_sig,
    id,
    model,
    parameters,
    snapped_zenith_coord,
    horizon_radius,
    is_horizon_circle_clipped,
    max_zenith_angle = NULL,
    date = NULL,
    notes = NULL,
    contact_information = NULL
) {

  .check_registry_entry(registry_entry)
  if (is.null(date)) {
    date <- Sys.Date()
  }
  .check_vector(embedded_metadata_sig, "character", 1)
  if (is.null(registry_entry[[embedded_metadata_sig]])) {
    stop(
      sprintf("metadata signature with id '%s' does not exist in this registry entry.", embedded_metadata_sig),
      call. = FALSE
    )
  }
  .embedded_metadata_sig <- registry_entry[[embedded_metadata_sig]]
  .check_embedded_metadata_sig(.embedded_metadata_sig)
  .check_vector(id, "character", 1)
  if (!is.null(.embedded_metadata_sig$geometry[[id]])) {
    stop(
      sprintf(
        "Geometry spec with id '%s' already exists for embedded metadata signature '%s'.",
        id, embedded_metadata_sig
      ),
      call. = FALSE
    )
  }

  geometry_spec <- .new_geometry_spec(
    id = id,
    model = model,
    parameters = parameters,
    snapped_zenith_coord = snapped_zenith_coord,
    horizon_radius = horizon_radius,
    is_horizon_circle_clipped = is_horizon_circle_clipped,
    max_zenith_angle = max_zenith_angle,
    date = date,
    notes = notes,
    contact_information = contact_information
  )

  .validate_geometry_spec <- function(geometry_spec) {

    if (is.null(geometry_spec$is_horizon_circle_clipped)) {
      return(invisible(TRUE))
    }

    if (geometry_spec$is_horizon_circle_clipped) {
      return(invisible(TRUE))
    }

    zenith_coord <- geometry_spec$snapped_zenith_coord
    radius <- geometry_spec$horizon_radius
    dim    <- registry_entry[[embedded_metadata_sig]]$dim

    ## Bounds of the circle
    left   <- zenith_coord[1] - radius
    right  <- zenith_coord[1] + radius
    top    <- zenith_coord[2] - radius
    bottom <- zenith_coord[2] + radius

    if (left < 0 || top < 0 || right > dim[1] || bottom > dim[2]) {
      msn <- paste0(
        "Invalid geometry specification: `is_horizon_circle_clipped = FALSE`, ",
        "but the extrapolated horizon circle (90 deg zenith angle) exceeds the image bounds. ",
        "Set `is_horizon_circle_clipped = TRUE` if horizon clipping is expected."
      )
      stop(msn, call. = FALSE)
    }

    invisible(TRUE)
  }
  .validate_geometry_spec(geometry_spec)

  registry_entry[[embedded_metadata_sig]]$geometry[[id]] <- geometry_spec
  registry_entry
}
