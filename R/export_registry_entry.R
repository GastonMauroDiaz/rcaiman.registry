#' Export and read registry snapshots
#'
#' @description
#' Create and read portable snapshots of registry entries stored as YAML files.
#'
#' @details
#' A registry snapshot is a YAML representation of a specific subset of
#' an [hs_registry_entry] object. It captures a single combination of
#' [embedded_metadata_sig], [geometry_spec], and [radiometry_spec], together
#' with minimal metadata about the exporting environment.
#'
#' This mechanism provides a lightweight way to declare image
#' geometric projection and radiometric correction without requiring the full
#' registry infrastructure.
#'
#' The function [export_registry_snapshot()] writes a snapshot file, while
#' [read_registry_snapshot()] reconstructs an [hs_registry_entry] object from
#' such file. The read functionality makes this format also useful to store and
#' distribute single-banch registry estries.
#'
#' @param file character vector of length one. Path to the YAML file used to
#'   write or read the snapshot.
#'
#' @inheritParams add_file_sig
#' @inheritParams get_embedded_metadata_sig
#' @inheritParams get_geometry_spec
#' @inheritParams get_radiometry_spec
#'
#' @return
#'
#' `export_registry_snapshot()` no return value. Called for side effects.
#'
#' `read_registry_snapshot()` returns an object of class [`hs_registry_entry`].
#'
#' @seealso
#' [`add_embedded_metadata_sig()`], [`add_geometry_spec()`],
#' [`add_radiometry_spec()`]
#'
#' @rdname registry-snapshots
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Build the object to export ----------------------------------------------
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
#' foo <- add_radiometry_spec(
#'   foo,
#'   embedded_metadata_sig = "exif_01",
#'   geometry_spec = "simple_method",
#'   id = "simple_method",
#'   type = "flat_field_correction",
#'   model = "flat_field_simple_polynomial",
#'   parameters = list("5.0" = c(0.0638, -0.101)),
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' spec <- get_geometry_spec(foo,
#'                           "exif_01", "simple_method")
#' z <- zenith_image(spec$horizon_radius*2, spec$parameters)
#' a <- azimuth_image(z)
#' files <- system.file("external/flat_field_image.tif", package = "rcaiman.registry")
#' sampling_points <- fibonacci_points(z, a, 10)
#' fnumber <- c(5)
#' radiance <- list()
#' for (i in seq_along(files)) {
#'   r <- read_caim(files[i])
#'   sp <- lapply(1:4, function(i) extract_rr(r[[i]], z, a, sampling_points)$sky_points)
#'   df <- lapply(sp, function(x) x[,"rr"]) %>% data.frame()
#'   names(df) <- names(r)
#'   radiance[[i]] <- df
#' }
#' names(radiance) <- fnumber
#' angles <- sp[[1]][, c("z", "a")]
#' names(angles) <- c("zenith", "azimuth")
#' data <- list(angles, radiance)
#' names(data) <- c("angles", "relative_radiance")
#'
#'
#' foo <- add_radiometry_spec(
#'   foo,
#'   embedded_metadata_sig = "exif_01",
#'   geometry_spec = "simple_method",
#'   id = "flat_field_generation",
#'   type = "flat_field_correction",
#'   model = "flat_field_trend_surface",
#'   data = data,
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' # -------------------------------------------------------------------------
#'
#' export_registry_snapshot(foo,
#'                          "exif_01",
#'                          "simple_method",
#'                          "flat_field_generation",
#'                          "registry_snapshot.yaml")
#' }
#'
#' read_registry_snapshot(system.file("external/registry_snapshot.yaml", package = "rcaiman.registry"))
#'
export_registry_snapshot <- function(
    registry_entry,
    embedded_metadata_sig,
    geometry_spec,
    radiometry_spec,
    file
) {

  .sanitize_snapshot <- function(x) {

    if (inherits(x, "Date")) {
      return(as.character(x))
    }

    if (is.double(x) && all(is.na(x) | x == floor(x))) {
      return(as.integer(x))
    }

    if (is.list(x)) {
      return(lapply(x, .sanitize_snapshot))
    }

    x
  }
  .check_registry_entry(registry_entry)

  instrument_metadata <- registry_entry$instrument_metadata

  em <- get_embedded_metadata_sig(registry_entry, embedded_metadata_sig)
  fs <- if (!is.null(em$file_sig)) {
    get_file_sig(registry_entry, em$file_sig)
  } else {
    NULL
  }
  geom <- get_geometry_spec(em, geometry_spec)
  rad <- get_radiometry_spec(geom, radiometry_spec)

  em$geometry <- NULL
  geom$radiometry <- NULL

  snapshot <- list(
    schema_version = 1,
    created_with = list(
      package = "rcaiman.registry",
      version = as.character(utils::packageVersion("rcaiman.registry")),
      date = as.character(Sys.Date())
    ),
    configuration = list(
      embedded_metadata_sig = em$id,
      geometry_spec = geom$id,
      radiometry_spec = rad$id
    ),
    registry_entry = if (is.null(fs)) {
      list(
        instrument_metadata = instrument_metadata,
        embedded_metadata_sig = em,
        geometry_spec = geom,
        radiometry_spec = rad
      )
    } else {
      list(
        instrument_metadata = instrument_metadata,
        file_sig = fs,
        embedded_metadata_sig = em,
        geometry_spec = geom,
        radiometry_spec = rad
      )
    }
  )

  snapshot <- .sanitize_snapshot(snapshot)

  yaml::write_yaml(snapshot, file, indent.mapping.sequence = TRUE)

  invisible(file)
}


#' @rdname registry-snapshots
#' @export
read_registry_snapshot <- function(file) {
  .assert_file_exists(file)

  snapshot <- yaml::read_yaml(file)

  entry <- snapshot$registry_entry

  im <- entry$instrument_metadata
  fs <- entry$file_sig
  em <- entry$embedded_metadata_sig
  geom <- entry$geometry_spec
  rad <- entry$radiometry_spec



# Fix radiometry spec -----------------------------------------------------

  .reconstruct_flat_field_data <- function(data) {

    if (is.null(data))
      return(NULL)

    # --- angles ---
    if (is.list(data$angles) && !is.data.frame(data$angles)) {
      data$angles <- as.data.frame(data$angles, optional = TRUE)
    }

    # --- radiance ---
    rr <- data$relative_radiance

    if (is.list(rr)) {
      data$relative_radiance <- lapply(
        rr,
        function(x) {
          if (is.list(x) && !is.data.frame(x)) {
            as.data.frame(x, optional = TRUE)
          } else {
            x
          }
        }
      )
    }

    data
  }
  if (!is.null(rad$data)) {
    rad$data <- .reconstruct_flat_field_data(rad$data)
    .check_flat_field_data(rad$data)
  }

# -------------------------------------------------------------------------

  class(geom) <- "geometry_spec"
  class(em) <- "embedded_metadata_sig"
  class(rad) <- "radiometry_spec"

  if (!is.null(fs)) {
    class(fs) <- "file_sig"
    fs_id <- fs$id
  }

  em_id <- em$id
  geom_id <- geom$id
  rad_id <- rad$id

  geom$radiometry <- list()
  geom$radiometry[[rad_id]] <- rad

  em$geometry <- list()
  em$geometry[[geom_id]] <- geom

  registry_entry <- list(
    instrument_metadata = im
  )
  if (!is.null(fs)) registry_entry[[fs_id]] <- fs
  registry_entry[[em_id]] <- em

  class(registry_entry) <- "hs_registry_entry"

  registry_entry
}
