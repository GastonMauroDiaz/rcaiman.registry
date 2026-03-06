.new_file_sig <- function(
    id,
    extension,
    filename_pattern = NULL,
    date = NULL
) {
  .assert_id(id)
  .check_vector(extension, "character", 1)
  .check_vector(filename_pattern, "character", 1, allow_null = TRUE)
  .check_vector(date, "date", 1)
  x <- list(
    id = id,
    extension = extension,
    filename_pattern = filename_pattern,
    date = date
  )
  class(x) <- "file_sig"
  x
}

#' Add a file signature to an existing registry entry
#'
#' @description
#' Add a [file_sig] to a [hs_registry_entry] object.
#'
#' @param extension character vector of length one. Expected file extension
#'   (e.g. "CR2", "NEF", "DNG").
#' @param filename_pattern optional character vector of length one. Usual
#'   filename structure *excluding the file extension*, typically expressed as a
#'   regular expression describing the default camera-generated basename. (e.g.
#'   `"^IMG_[0-9]{4}$"` matches basenames such as \code{IMG_0001},
#'   \code{IMG_0234}).
#'
#' @inheritParams add_embedded_metadata_sig
#' @inheritParams add_geometry_spec
#'
#' @return
#' A [hs_registry_entry] object with the [file_sig] appended.
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
#' foo <- add_file_sig(
#'   foo,
#'   id = "raw",
#'   extension = "NEF",
#'   filename_pattern = "^DSCN[0-9]{4}$"
#' )
#'
#' foo
#' summary(foo)
add_file_sig <- function(
    registry_entry,
    id,
    extension,
    filename_pattern = NULL,
    date = NULL
) {

  .check_registry_entry(registry_entry)
  if (is.null(date)) {
    date <- Sys.Date()
  }
  .check_vector(id, "character", 1)
  if (!is.null(registry_entry[[id]])) {
    stop(
      sprintf("A file signature with id '%s' already exists in this registry entry.", id),
      call. = FALSE
    )
  }

  file_sig <- .new_file_sig(
    id = id,
    extension = extension,
    filename_pattern = filename_pattern,
    date = date
  )

  registry_entry[[id]] <- file_sig
  registry_entry
}
