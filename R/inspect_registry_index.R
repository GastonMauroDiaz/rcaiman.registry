#' Inspect registry entry structure
#'
#' @description
#' Return a hierarchical index of the components stored in a
#' registry entry.
#'
#' @inheritParams add_embedded_metadata_sig
#'
#' @return
#' A named list describing the structural organization of the registry entry.
#'
#' @export
#' @examples
#' foo <- new_registry_entry(
#' "Nikon_Coolpix5700.FCE9.CIEFAP",
#' body = "E5700",
#' body_manufacturer = "NIKON CORP",
#' body_serial = "7053067",
#' lens = "Zoom Nikkor ED 8.9-71.2mm 1:2.8-4.2",
#' lens_manufacturer = "NIKON CORP",
#' auxiliary_lens = "Fisheye Converter FC-E9 0.2x",
#' auxiliary_lens_manufacturer = "NIKON CORP",
#' institution = "CIEFAP"
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
#' foo <- add_radiometry_spec(
#'   foo,
#'   embedded_metadata_sig = "exif_raw",
#'   geometry_spec = "simple_method",
#'   id = "spectral_bands",
#'   type = "interpretive_constraint",
#'   cfa_pattern = matrix(c("Green", "Yellow",
#'                          "Magenta", "Cyan"), byrow = TRUE, ncol = 2),
#'   spectral_mapping = list(Red = "(Yellow + Magenta)/2",
#'                           Green = "Green",
#'                           Blue = "Cyan"),
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' foo <- add_radiometry_spec(
#'   foo,
#'   embedded_metadata_sig = "exif_raw",
#'   geometry_spec = "simple_method",
#'   id = "simple_method",
#'   type = "flat_field_correction",
#'   model = "ff_simple_polynomial",
#'   parameters = list("5.0" = c(0.0638, -0.101)),
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' foo
#' summary(foo)
#' inspect_registry_index(foo)
inspect_registry_index <- function(registry_entry) {

  .check_registry_entry(registry_entry)

  components <- registry_entry[names(registry_entry) != "instrument_metadata"]

  is_embedded <- vapply(components, .is_embedded_metadata_sig, logical(1))

  embedded_metadata_sig_ids <- names(components)[is_embedded]

  embedded_index <- vector("list", length(embedded_metadata_sig_ids))
  names(embedded_index) <- embedded_metadata_sig_ids

  for (ems in embedded_metadata_sig_ids) {

    em <- components[[ems]]

    geometry_spec_ids <- character(0)
    radiometry_spec_ids <- list()

    if (!is.null(em$geometry) && is.list(em$geometry)) {

      geometry_spec_ids <- names(em$geometry)

      for (gs in geometry_spec_ids) {

        g <- em$geometry[[gs]]

        if (!is.null(g$radiometry) && is.list(g$radiometry)) {
          radiometry_spec_ids[[gs]] <- names(g$radiometry)
        } else {
          radiometry_spec_ids[[gs]] <- character(0)
        }
      }
    }

    embedded_index[[ems]] <- list(
      geometry_spec_ids = geometry_spec_ids,
      radiometry_spec_ids = radiometry_spec_ids
    )
  }

  list(
    embedded_metadata_sig = embedded_index
  )
}
