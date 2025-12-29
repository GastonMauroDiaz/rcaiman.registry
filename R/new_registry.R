#' Construct a hemispherical camera registry entry
#'
#' @description
#' Construct a registry object describing a hemispherical camera system and its
#' physical components (camera body, device, lens, and/or auxiliary lens).
#'
#' @details
#' A hemispherical camera registry represents a *physical acquisition system*
#' rather than a single device. Depending on the configuration, the system may
#' correspond to typical acquisition setups such as:
#'
#' \itemize{
#'   \item an image acquisition device with or without an auxiliary hemispherical lens,
#'   \item a compact camera (camera body with a fixed lens) with an auxiliary lens,
#'   \item a reflex or mirrorless camera body with an interchangeable lens.
#' }
#'
#' For this reason, information related to camera bodies, devices, lenses, and
#' auxiliary lenses is stored internally as separate components, each with its
#' own identity (model, manufacturer, serial number).
#'
#' The returned object is a plain R list with class
#' `"hemispherical_camera_registry"`. After creation, it can be inspected and
#' edited manually using base R tools if needed.
#'
#' Calibration information must be added explicitly using [add_geometry_spec()]
#' and [add_radiometry_spec()], allowing multiple calibrations to be associated
#' with the same instrument over time.
#'
#' Objects stored in a registry are assumed to be technically valid.
#'
#' @param id character vector of length one. Unique identifier of the camera
#'   system within a project, institution, or personal workflow.
#' @param body optional character vector of length one. Camera body model.
#' @param body_manufacturer optional character vector of length one.
#' @param body_serial optional character vector of length one.
#' @param device optional character vector of length one. Image acquisition
#'   device used with an auxiliary lens (e.g. smartphone model).
#' @param device_manufacturer optional character vector of length one.
#' @param device_serial optional character vector of length one.
#' @param lens optional character vector of length one. Primary lens model.
#' @param lens_manufacturer optional character vector of length one.
#' @param lens_serial optional character vector of length one.
#' @param auxiliary_lens optional character vector of length one. Auxiliary
#'   fisheye lens.
#' @param auxiliary_lens_manufacturer optional character vector of length one.
#' @param auxiliary_lens_serial optional character vector of length one.
#' @param institution optional character vector of length one. Institution where
#'   the instrument is usually stored.
#'
#' @note
#' This object is intentionally simple and can be edited manually using base R
#' tools. Manual edits may break internal assumptions used by downstream
#' functions.
#'
#' @return An object of class \link{hemispherical_camera_registry}.
#'
#' @export
new_registry <- function(
    id,
    body = NULL,
    body_manufacturer = NULL,
    body_serial = NULL,
    device = NULL,
    device_manufacturer = NULL,
    device_serial = NULL,
    lens = NULL,
    lens_manufacturer = NULL,
    lens_serial = NULL,
    auxiliary_lens = NULL,
    auxiliary_lens_manufacturer = NULL,
    auxiliary_lens_serial = NULL,
    institution = NULL
) {

  .check_vector(id, "character", 1)
  .check_vector(body, "character", 1, allow_null = TRUE)
  .check_vector(body_manufacturer, "character", 1, allow_null = TRUE)
  .check_vector(body_serial, "character", 1, allow_null = TRUE)
  .check_vector(device, "character", 1, allow_null = TRUE)
  .check_vector(device_manufacturer, "character", 1, allow_null = TRUE)
  .check_vector(device_serial, "character", 1, allow_null = TRUE)
  .check_vector(lens, "character", 1, allow_null = TRUE)
  .check_vector(lens_manufacturer, "character", 1, allow_null = TRUE)
  .check_vector(lens_serial, "character", 1, allow_null = TRUE)
  .check_vector(auxiliary_lens, "character", 1, allow_null = TRUE)
  .check_vector(auxiliary_lens_manufacturer, "character", 1, allow_null = TRUE)
  .check_vector(auxiliary_lens_serial, "character", 1, allow_null = TRUE)

  has_body   <- !is.null(body)
  has_device <- !is.null(device)
  has_lens   <- !is.null(lens)
  has_aux    <- !is.null(auxiliary_lens)

  case_compact    <- has_body && has_aux && !has_device
  case_reflex     <- has_body && has_lens && !has_device && !has_aux

  if (!(has_device || case_compact || case_reflex)) {
    stop(
      paste(
        "Invalid hemispherical camera configuration.",
        "Valid configurations include:",
        "- `device`, optionally combined with an `auxiliary_lens`",
        "- `body` combined with an `auxiliary_lens` (optionally with `lens`)",
        "- `body` combined with `lens`",
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  instrument_metadata <- list(
    id = id,

    body = if (has_body) list(
      model = body,
      manufacturer = body_manufacturer,
      serial = body_serial
    ) else NULL,

    device = if (has_device) list(
      model = device,
      manufacturer = device_manufacturer,
      serial = device_serial
    ) else NULL,

    lens = if (has_lens) list(
      model = lens,
      manufacturer = lens_manufacturer,
      serial = lens_serial
    ) else NULL,

    auxiliary_lens = if (has_aux) list(
      model = auxiliary_lens,
      manufacturer = auxiliary_lens_manufacturer,
      serial = auxiliary_lens_serial
    ) else NULL,

    institution = institution
  )

  x <- list(
    instrument_metadata = instrument_metadata
  )

  class(x) <- "hemispherical_camera_registry"
  x
}
