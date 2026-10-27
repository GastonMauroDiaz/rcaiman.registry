#' Export and read registry snapshots
#'
#' @description
#' Write a selected registry branch to YAML and reconstruct it as a registry
#' entry.
#'
#' @details
#' A snapshot contains instrument metadata, one [signature], one
#' [band_spec], and optionally one [geometry_spec] and one flat-field
#' [radiometry_spec]. A snapshot can end at the band specification, preserving
#' its interpretation without selecting a geometry. Radiometry requires a
#' selected geometry. Other branches are not exported.
#'
#' The reader rebuilds the branch through the specification constructors,
#' restoring dates, the CFA matrix, and model parameters. The resulting
#' geometry can be passed directly to [rcaiman::set_internal_geometry()];
#' radiometry parameters can be passed to their declared core evaluator.
#'
#' Snapshots use schema version 4. Earlier schemas are not supported. Numeric
#' values are written with 17 significant digits. A CFA matrix is stored as
#' column-major `values` and optional `dimnames`; its dimensions are always
#' 2 x 2. Spectral expressions are stored as text and are not evaluated by
#' the reader.
#'
#' @param file character vector of length one. Path to the YAML file.
#' @param radiometry_spec optional character vector of length one.
#'   Identifier of a [radiometry_spec] under the selected geometry. If `NULL`,
#'   the snapshot contains no radiometry specification. Requires `geometry_spec`.
#' @param registry_entry [hs_registry_entry] to export.
#' @param signature character vector of length one. Identifier
#'   of the selected metadata signature.
#' @param band_spec character vector of length one. Identifier of the selected
#'   band specification within that metadata signature.
#' @param geometry_spec optional character vector of length one. Identifier of
#'   the selected geometry within that band specification. If `NULL`, the
#'   snapshot ends at the band specification.
#'
#' @return
#' `export_registry_snapshot()` invisibly returns `file`.
#'
#' `read_registry_snapshot()` returns an [hs_registry_entry] with the selected
#' metadata signature and band, plus geometry and radiometry when selected.
#'
#' @seealso [add_band_spec()], [add_geometry_spec()], [add_radiometry_spec()]
#' @rdname registry-snapshots
#' @export
#' @examples
#' entry <- read_registry_snapshot(system.file(
#'   "external/my_entry_v2.yaml", package = "rcaiman.registry"
#' ))
#' summary(entry)
#' file <- tempfile(fileext = ".yaml")
#' export_registry_snapshot(
#'   entry,
#'   "raw",
#'   "cygm",
#'   "simple_method",
#'   radiometry_spec = "simple_method",
#'   file = file
#' )
#' restored <- read_registry_snapshot(file)
#' summary(restored)
#'
#' # Export band interpretation without geometry or radiometry.
#' export_registry_snapshot(entry, "raw", "cygm", file = file)
#' bands_only <- read_registry_snapshot(file)
#' summary(bands_only)
#' unlink(file)
export_registry_snapshot <- function(
    registry_entry, signature, band_spec, geometry_spec = NULL,
    radiometry_spec = NULL, file
) {
  .check_registry_entry(registry_entry)
  .check_vector(geometry_spec, "character", 1, allow_null = TRUE)
  .check_vector(radiometry_spec, "character", 1, allow_null = TRUE)
  .check_vector(file, "character", 1)
  if (is.null(geometry_spec) && !is.null(radiometry_spec)) {
    stop("`radiometry_spec` requires `geometry_spec`.", call. = FALSE)
  }
  ems <- get_signature(registry_entry, signature)
  band <- get_band_spec(ems, band_spec)
  geometry <- if (!is.null(geometry_spec))
    get_geometry_spec(band, geometry_spec) else NULL
  radiometry <- if (!is.null(radiometry_spec))
    get_radiometry_spec(geometry, radiometry_spec) else NULL

  ems$bands <- NULL
  band$geometry <- NULL
  if (!is.null(geometry)) geometry$radiometry <- NULL
  if (!is.null(band$cfa_pattern)) {
    band$cfa_pattern <- list(values = as.vector(band$cfa_pattern),
                             dimnames = dimnames(band$cfa_pattern))
  }
  snapshot <- list(
    schema_version = 4L,
    created_with = list(package = "rcaiman.registry",
                        version = as.character(utils::packageVersion("rcaiman.registry")),
                        date = as.character(Sys.Date())),
    configuration = list(signature = ems$id, band_spec = band$id,
                          geometry_spec = geometry$id,
                          radiometry_spec = radiometry_spec),
    registry_entry = list(instrument_metadata = registry_entry$instrument_metadata,
                          signature = ems, band_spec = band,
                          geometry_spec = geometry,
                          radiometry_spec = radiometry)
  )
  encode <- function(x) {
    if (inherits(x, "Date")) return(as.character(x))
    # YAML sequences discard names on atomic vectors; mappings retain them.
    if (is.atomic(x) && !is.null(names(x))) x <- as.list(x)
    if (is.list(x)) return(lapply(x, encode))
    x
  }
  yaml::write_yaml(encode(snapshot), file, indent.mapping.sequence = TRUE,
                   precision = 17)
  invisible(file)
}

#' @rdname registry-snapshots
#' @export
read_registry_snapshot <- function(file) {
  .assert_file_exists(file)
  snapshot <- yaml::read_yaml(file, eval.expr = FALSE)
  if (!is.list(snapshot) || length(snapshot$schema_version) != 1L ||
      is.na(snapshot$schema_version) || snapshot$schema_version != 4) {
    stop("Unsupported registry snapshot schema; expected version 4.", call. = FALSE)
  }
  entry <- snapshot$registry_entry
  required <- c("instrument_metadata", "signature", "band_spec",
                "geometry_spec", "radiometry_spec")
  if (!is.list(entry) || !all(required %in% names(entry))) {
    stop("Incomplete registry snapshot branch.", call. = FALSE)
  }
  ems <- entry$signature
  band <- entry$band_spec
  geometry <- entry$geometry_spec
  radiometry <- entry$radiometry_spec
  if (is.null(geometry) && !is.null(radiometry)) {
    stop("Snapshot radiometry requires a geometry specification.",
         call. = FALSE)
  }
  ids <- list(signature = ems$id, band_spec = band$id,
               geometry_spec = geometry$id, radiometry_spec = radiometry$id)
  if (!is.list(snapshot$configuration) ||
      !identical(snapshot$configuration[names(ids)], ids)) {
    stop("Snapshot configuration does not match the selected branch.", call. = FALSE)
  }

  im <- entry$instrument_metadata
  args <- list(id = im$id, institution = im$institution)
  for (part in c("body", "device", "lens", "auxiliary_lens")) {
    if (!is.null(im[[part]])) {
      args[[part]] <- im[[part]]$model
      args[paste0(part, "_manufacturer")] <- list(im[[part]]$manufacturer)
      args[paste0(part, "_serial")] <- list(im[[part]]$serial)
    }
  }
  result <- do.call(new_registry_entry, args)
  ems$date <- as.Date(ems$date)
  ems$dim <- unlist(ems$dim)
  ems <- do.call(.new_signature, ems)

  band$date <- as.Date(band$date)
  if (!is.null(band$cfa_pattern)) {
    cfa <- band$cfa_pattern
    if (length(cfa$values) != 4L) {
      stop("Snapshot CFA pattern must contain four values.", call. = FALSE)
    }
    band$cfa_pattern <- matrix(cfa$values, nrow = 2, dimnames = cfa$dimnames)
  }
  band <- do.call(.new_band_spec, band)

  if (!is.null(geometry)) {
    geometry$date <- as.Date(geometry$date)
    geometry$center_of_vision <- unlist(geometry$center_of_vision)
    if (identical(geometry$model, "radial_projection")) {
      geometry$parameters <- unlist(geometry$parameters)
    } else if (identical(geometry$model, "anisotropic_projection")) {
      geometry$parameters <- lapply(geometry$parameters, unlist)
    }
    geometry <- do.call(.new_geometry_spec, geometry)
  }

  if (!is.null(radiometry)) {
    radiometry$date <- as.Date(radiometry$date)
    if (!is.null(radiometry$parameters)) {
      radiometry$parameters <- lapply(radiometry$parameters, function(bands) {
        lapply(bands, function(p) {
        if (identical(radiometry$model, "trend_surface_image")) {
          lapply(p, unlist)
        } else unlist(p)
        })
      })
    }
    radiometry <- do.call(.new_radiometry_spec, radiometry)
    geometry$radiometry[[radiometry$id]] <- radiometry
  }
  if (!is.null(geometry)) band$geometry[[geometry$id]] <- geometry
  ems$bands[[band$id]] <- band
  result[[ems$id]] <- ems
  result
}
