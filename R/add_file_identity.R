#' Add a file identity signature to an existing registry
#'
#' @description
#' Add a file-based identity signature describing the expected external
#' properties of image files produced by a given instrumental system.
#'
#' @details
#' A file identity defines expected *file-level properties* of image files
#' as digital artifacts produced by an acquisition system, independently
#' of their embedded metadata.
#'
#' For a conceptual overview of registry structure, see
#' [hemispherical_camera_registry].
#'
#' File identities are intended to capture stable conventions imposed by
#' acquisition systems, such as file extensions or default filename patterns.
#'
#' This function does not perform any validation or matching. It only
#' registers declarative expectations that may later be used to support
#' traceability, reproducibility, or automated consistency checks.
#'
#' @param extension character vector of length one. Expected file extension
#'   (e.g. "CR2", "NEF", "DNG").
#' @param filename_pattern optional character vector of length one. Expected
#'   filename structure *excluding the file extension*, typically expressed as a
#'   regular expression describing the default camera-generated basename. (e.g.
#'   `"^IMG_[0-9]{4}$"` matches basenames such as \code{IMG_0001},
#'   \code{IMG_0234})
#'
#' @inheritParams add_geometry_spec
#'
#' @return
#' A `hemispherical_camera_registry` object with the file identity appended.
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
#' foo <- add_file_identity(
#'   foo,
#'   id = "raw",
#'   extension = "NEF",
#'   filename_pattern = "^DSC_[0-9]{4}$"
#' )
#'
add_file_identity <- function(
    registry,
    id,
    extension,
    filename_pattern = NULL,
    date = NULL,
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
      sprintf("A file identity with id '%s' already exists in this registry.", id),
      call. = FALSE
    )
  }
  .assert_id(id)
  .check_vector(extension, "character", 1)
  .check_vector(filename_pattern, "character", 1, allow_null = TRUE)
  .check_vector(date, "date", 1)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  file_identity <- list(
    id = id,
    extension = extension,
    filename_pattern = filename_pattern,
    date = date,
    notes = notes,
    contact_information = contact_information
  )

  registry[[id]] <- file_identity
  registry
}
