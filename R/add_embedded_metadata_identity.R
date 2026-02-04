#' Add an embedded metadata identity signature to an existing registry
#'
#' @description
#' Add an identity signature describing the expected
#' metadata signatures of image files produced by a given instrumental
#' system.
#'
#' @details
#' An embedded metadata identity signature defines an expected metadata signature
#' associated with image files produced by a given instrumental system.
#'
#' For a conceptual overview of registry structure, see
#' [hemispherical_camera_registry].
#'
#' An embedded metadata identity consists of a declarative set of metadata
#' tag–value pairs (`rules`) that may
#' later be evaluated against the metadata of a given image.
#'
#' This function does not perform any validation or matching. It only
#' registers the metadata expectations required to assess compatibility
#' at a later stage. Their role is to support traceability, reproducibility,
#' and automated consistency checks.
#'
#' @param namespace character vector of length one. Lowercase identifier of the
#'   embedded metadata namespace (e.g. "exif", "xmp"). This value is descriptive
#'   and does not trigger any validation at registration time.
#' @param rules named list composed of character vectors of length one. Expected
#'   metadata values. Names must correspond to tag names.
#'
#' @inheritParams add_geometry_spec
#'
#' @return
#' A `hemispherical_camera_registry` object with the embedded metadata identity appended.
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
#' foo <- add_embedded_metadata_identity(
#'   foo,
#'   id = "exif_01",
#'   namespace = "exif",
#'   rules = list(
#'     "Camera Model Name" = "NIKON D610",
#'     "Serial Number" = "9023728",
#'     "Lens" = "8-15mm f/3.5-4.5",
#'     "Firmware Version" = "1.01",
#'     "CFA Pattern" = "[Red,Green][Green,Blue]"
#'   ),
#'   firmware_version = "1.01",
#'   notes = "Taken with ExifTool Version Number 12.92",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
add_embedded_metadata_identity <- function(
    registry,
    id,
    namespace,
    rules,
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

  if (!is.null(registry[[id]])) {
    stop(
      sprintf("An embedded metadata identity with id '%s' already exists in this registry.", id),
      call. = FALSE
    )
  }

  .check_vector(id, "character", 1)
  .assert_id(id)
  .check_vector(namespace, "character", 1)
  .assert_id(namespace)
  .check_vector(date, "date", 1)
  .check_vector(firmware_version, "character", 1, allow_null = TRUE)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  if (length(rules) == 0) {
    stop("`rules` must contain at least one tag expectation.", call. = FALSE)
  }

  if (!is.list(rules) || is.null(names(rules))) {
    stop(
      "`rules` must be a named list of tag expectations.",
      call. = FALSE
    )
  }

  exif_identity <- list(
    id = id,
    namespace = namespace,
    rules = rules,
    date = date,
    notes = notes,
    contact_information = contact_information
  )

  registry[[id]] <- exif_identity
  registry
}
