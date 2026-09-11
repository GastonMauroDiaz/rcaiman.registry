#' Export and read registry snapshots
#'
#' @description
#' Create and read portable snapshots of registry entries stored as YAML files.
#'
#' @details
#' A registry snapshot is a YAML representation of a specific subset of
#' an [hs_registry_entry] object. It captures a single combination of
#' [embedded_metadata_sig], [geometry_spec], and [radiometry_spec] with
#' `type = "interpretive_constraint` and with `type = "flat_field_correction"`,
#' together with intrument metadata and minimal metadata about the
#' exporting environment.
#'
#' This mechanism provides a lightweight way to declare image geometric
#' projection and radiometric correction without requiring the full registry
#' infrastructure. In this way, the YAML file can act as the metadata of
#' pre-processed images.
#'
#' Moreover, [read_registry_snapshot()] reconstructs an [hs_registry_entry]
#' object from a snapshot YAML file, making this format also useful to store and
#' distribute single-branch registry estries.
#'
#' @param file character vector of length one. Path to the YAML file used to
#'   write or read the snapshot.
#' @param interpretive_constraint optional character vector of length one.
#'   Identifier of a [radiometry_spec] of type `interpretive_constraint`.
#' @param flat_field_correction optional character vector of length one.
#'   Identifier of a [radiometry_spec] of type `flat_field_correction`.
#'
#' @inheritParams get_embedded_metadata_sig
#' @inheritParams get_geometry_spec
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
#' # Export the snapshot -----------------------------------------------------
#'
#' \dontrun{
#' export_registry_snapshot(foo,
#'                          "exif_raw",
#'                          "simple_method",
#'                          "spectral_bands",
#'                          "simple_method",
#'                          "inst/external/registry_snapshot.yaml")
#' }
#'
#' # Read the snapshot -------------------------------------------------------
#'
#' read_registry_snapshot(system.file("external/registry_snapshot.yaml", package = "rcaiman.registry"))
#'
export_registry_snapshot <- function(
    registry_entry,
    embedded_metadata_sig,
    geometry_spec,
    interpretive_constraint = NULL,
    flat_field_correction = NULL,
    file
) {
   .check_registry_entry(registry_entry)
   .check_vector(embedded_metadata_sig, "character", 1)
   .check_vector(geometry_spec, "character", 1)
   .check_vector(interpretive_constraint, "character", 1, allow_null = TRUE)
   .check_vector(flat_field_correction, "character", 1, allow_null = TRUE)
   .check_vector(file, "character", 1)


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

  ems <- get_embedded_metadata_sig(registry_entry, embedded_metadata_sig)
  spec <- get_geometry_spec(ems, geometry_spec)
  rspec_ic <- if (!is.null(interpretive_constraint))
    get_radiometry_spec(spec, interpretive_constraint)
  else
    NULL
  rspec_ffc <- if (!is.null(flat_field_correction))
    get_radiometry_spec(spec, flat_field_correction)
  else
    NULL

  ems$geometry <- NULL
  spec$radiometry <- NULL

  snapshot <- list(
    schema_version = 1,
    created_with = list(
      package = "rcaiman.registry",
      version = as.character(utils::packageVersion("rcaiman.registry")),
      date = as.character(Sys.Date())
    ),
    configuration = list(
      embedded_metadata_sig = ems$id,
      geometry_spec = spec$id,
      interpretive_constraint = if (!is.null(interpretive_constraint))
        rspec_ic$id
      else
        NULL,
      flat_field_correction = if (!is.null(flat_field_correction))
        rspec_ffc$id
      else
        NULL
    ),
    registry_entry = list(
        instrument_metadata = instrument_metadata,
        embedded_metadata_sig = ems,
        geometry_spec = spec,
        interpretive_constraint = rspec_ic,
        flat_field_correction = rspec_ffc
      )
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
  ems <- entry$embedded_metadata_sig
  spec <- entry$geometry_spec
  rspec_ic <- entry$interpretive_constraint
  rspec_ffc <- entry$flat_field_correction


# Fix radiometry spec -----------------------------------------------------

  .reconstruct_flat_field_data <- function(data) {

    if (is.null(data))
      return(NULL)

    # --- direction ---
    if (is.list(data$direction) && !is.data.frame(data$direction)) {
      data$direction <- as.data.frame(data$direction, optional = TRUE)
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
  if (!is.null(rspec_ffc$data)) {
    rspec_ffc$data <- .reconstruct_flat_field_data(rspec_ffc$data)
    .validate_flat_field_data(rspec_ffc$data)
  }

# -------------------------------------------------------------------------

  class(spec) <- "geometry_spec"
  class(ems) <- "embedded_metadata_sig"

  if (!is.null(rspec_ic)) {
    class(rspec_ic) <- "radiometry_spec"
    #reconstruct matrix
    rspec_ic$cfa_pattern <- matrix(rspec_ic$cfa_pattern, ncol = 2)
    rspec_ic_id <- rspec_ic$id
  }

  if (!is.null(rspec_ffc)) {
    class(rspec_ffc) <- "radiometry_spec"
    rspec_ffc_id <- rspec_ffc$id
  }

  ems_id <- ems$id
  gspec_id <- spec$id

  spec$radiometry <- list()
  if (!is.null(rspec_ic)) spec$radiometry[[rspec_ic_id]] <- rspec_ic
  if (!is.null(rspec_ffc)) spec$radiometry[[rspec_ffc_id]] <- rspec_ffc

  ems$geometry <- list()
  ems$geometry[[gspec_id]] <- spec

  registry_entry <- list(
    instrument_metadata = im
  )
  registry_entry[[ems_id]] <- ems

  class(registry_entry) <- "hs_registry_entry"

  registry_entry
}
