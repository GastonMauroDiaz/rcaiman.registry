#' Extract radiometry specification
#'
#' @description
#' Retrieve a radiometry specification associated with a geometry specification
#' within a registry entry or not.
#'
#' @param x Either:
#'   - A [hs_registry_entry], or
#'   - A [geometry_spec].
#' @param embedded_metadata_sig Character vector of length one.
#'   Identifier of embedded metadata signature. Required when `x` is a
#'   `hs_registry_entry`.
#' @param geometry_spec Character vector of length one.
#'   Identifier of geometry specification. Required when `x` is a
#'   `hs_registry_entry`.
#' @param radiometry_spec Character vector of length one.
#'   Identifier of radiometry specification.
#' @param ... Additional arguments (unused).
#'
#' @return
#' An object of class [radiometry_spec].
#'
#' @details
#' When `x` is a [hs_registry_entry], the function navigates the hierarchy:
#'
#' `registry_entry, embedded_metadata_sig, geometry_spec, radiometry_spec`
#'
#' When `x` is a [geometry_spec], the radiometry specification is retrieved
#' directly from its `radiometry` container.
#'
#' @seealso
#' [add_radiometry_spec()], [get_geometry_spec()], [extract_radiometry()]
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
#' spec <- get_geometry_spec(foo, "exif_01", "simple_method")
#' rspec <- get_radiometry_spec(spec, "spectral_bands")
#' rspec <- get_radiometry_spec(foo, "exif_01", "simple_method", "spectral_bands")
get_radiometry_spec <- function(x, ...) {
  UseMethod("get_radiometry_spec")
}

#' @rdname get_radiometry_spec
#' @export
get_radiometry_spec.hs_registry_entry <- function(x,
                                                  embedded_metadata_sig,
                                                  geometry_spec,
                                                  radiometry_spec,
                                                  ...) {

  .check_registry_entry(x)
  .check_vector(embedded_metadata_sig, "character", 1)
  .check_vector(geometry_spec, "character", 1)
  .check_vector(radiometry_spec, "character", 1)

  g <- get_geometry_spec(
    x = x,
    embedded_metadata_sig = embedded_metadata_sig,
    geometry_spec = geometry_spec
  )

  get_radiometry_spec(g, radiometry_spec = radiometry_spec)
}

#' @rdname get_radiometry_spec
#' @export
get_radiometry_spec.geometry_spec <- function(x,
                                              radiometry_spec,
                                              ...) {

  .check_geometry_spec(x)
  .check_vector(radiometry_spec, "character", 1)

  if (!radiometry_spec %in% names(x$radiometry)) {
    stop(
      "Radiometry specification '",
      radiometry_spec,
      "' not found under geometry '",
      x$id,
      "'"
    )
  }

  x$radiometry[[radiometry_spec]]
}
