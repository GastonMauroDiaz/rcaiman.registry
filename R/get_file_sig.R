#' Extract file signature
#'
#' @description
#' Retrieve a file signature within a registry_entry.
#'
#' @param file_sig character vector of length one. Identifier of the [file_sig].
#'
#' @inheritParams add_file_sig
#'
#' @export
#'
#' @examples
#' # Build de object ---------------------------------------------------------
#'
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
#' foo <- add_embedded_metadata_sig(
#'   foo,
#'   id = "exif_01",
#'   namespace = "exif",
#'   dim = c(1288, 962),
#'   rules = list(
#'     "Camera Model Name" = "E5700",
#'     "Software" = "E5700v1.1",
#'     "CFA Pattern" = "[Yellow,Cyan][Green,Magenta]",
#'     "Compression" = "Uncompressed",
#'     "Bits Per Sample" = "12",
#'     "Quality" = "RAW"
#'   ),
#'   file_sig = "raw"
#' )
#'
#' foo <- add_geometry_spec(
#'   foo,
#'   embedded_metadata_sig = "exif_01",
#'   id = "simple_method",
#'   model = "radial_projection",
#'   parameters = c(0.6380,  0.0307, -0.0200),
#'   zenith_col_row = c(645, 494),
#'   horizon_radius = 378,
#'   is_horizon_circle_clipped = FALSE,
#'   max_zenith_angle = 94.8,
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' foo <- add_radiometry_spec(
#'   foo,
#'   embedded_metadata_sig = "exif_01",
#'   geometry_spec = "simple_method",
#'   id = "spectral_bands",
#'   type = "interpretive_constraint",
#'   cfa_pattern = matrix(c("Green", "Yellow",
#'                          "Magenta", "Cyan"), byrow = TRUE, ncol = 2),
#'   spectral_mapping = list(Red = "(Yellow + Magenta)/2",
#'                           Green = "Green",
#'                           Blue = "Cyan"),
#'   offset_value = list("100" = 0),
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' # -------------------------------------------------------------------------
#'
#' fs <- get_file_sig(foo, "raw")
#' em <- get_embedded_metadata_sig(foo, "exif_01")
#' fs <- get_file_sig(foo, em$file_sig)
get_file_sig <- function(registry_entry, file_sig) {

  .check_registry_entry(registry_entry)
  .check_vector(file_sig, "character", 1)

  if (!file_sig %in% names(registry_entry) ||
      !.is_file_sig(registry_entry[[file_sig]])) {
    stop("File signature '", file_sig, "' not found in this registry entry")
  }

  registry_entry[[file_sig]]
}
