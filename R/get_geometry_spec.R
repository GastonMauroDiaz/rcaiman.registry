#' Extract geometry specification
#'
#' @description
#' Retrieve a geometry specification associated with an embedded
#' metadata signature within a registry entry or not.
#'
#' @param x Either:
#'   - A [hs_registry_entry], or
#'   - An [embedded_metadata_sig].
#' @param embedded_metadata_sig Character vector of length one.
#'   Identifier of embedded metadata signature. Required when `x`
#'   is a `hs_registry_entry`.
#' @param geometry_spec Character vector of length one.
#'   Identifier of geometry specification.
#' @param ... Additional arguments (unused).
#'
#' @return
#' An object of class [geometry_spec].
#'
#' @details
#' When `x` is a [hs_registry_entry], the function navigates the hierarchy:
#'
#' `registry_entry, embedded_metadata_sig, geometry_spec`
#'
#' When `x` is an [embedded_metadata_sig], the geometry specification
#' is retrieved directly from its `geometry` container.
#'
#' @seealso
#' [add_geometry_spec()], [get_embedded_metadata_sig()], [get_radiometry_spec()]
#'
#' @export
#'
#' @examples
#'
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
#'   )
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
#' spec <- get_geometry_spec(foo, "exif_01", "simple_method")
#' em <- get_embedded_metadata_sig(foo, "exif_01")
#' spec <- get_geometry_spec(em, "simple_method")
get_geometry_spec <- function(x, ...) {
  UseMethod("get_geometry_spec")
}

#' @rdname get_geometry_spec
#' @export
get_geometry_spec.hs_registry_entry <- function(x,
                                                embedded_metadata_sig,
                                                geometry_spec,
                                                ...) {

  .check_registry_entry(x)
  .check_vector(embedded_metadata_sig, "character", 1)
  .check_vector(geometry_spec, "character", 1)

  em <- get_embedded_metadata_sig(
    registry_entry = x,
    embedded_metadata_sig = embedded_metadata_sig
  )

  get_geometry_spec(em, geometry_spec = geometry_spec)
}

#' @rdname get_geometry_spec
#' @export
get_geometry_spec.embedded_metadata_sig <- function(x,
                                                    geometry_spec,
                                                    ...) {

  .check_embedded_metadata_sig(x)
  .check_vector(geometry_spec, "character", 1)

  if (!geometry_spec %in% names(x$geometry)) {
    stop(
      "Geometry specification '",
      geometry_spec,
      "' not found under embedded metadata '",
      x$id,
      "'"
    )
  }

  x$geometry[[geometry_spec]]
}
