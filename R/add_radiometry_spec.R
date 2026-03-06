.new_radiometry_spec <- function(
    id,
    type,
    model = NULL,
    rcaiman_version = NULL,
    parameters = NULL,
    data = NULL,
    cfa_pattern = NULL,
    spectral_mapping = NULL,
    offset_value = NULL,
    date = NULL,
    notes = NULL,
    contact_information = NULL
) {

  .assert_id(id)
  .check_vector(type, "character", 1)
  .assert_choice(type, c("flat_field_correction", "interpretive_constraint"))

  if (type == "flat_field_correction") {
    .check_flat_field_model(model)
    if (is.null(rcaiman_version)) {
      rcaiman_version = as.character(utils::packageVersion("rcaiman"))
    }
    .check_vector(rcaiman_version, "character", 1)
    if (!is.null(parameters)) {
      if (!is.list(parameters)) {
        stop("`parameters` must be a list.")
      }
      nms <- names(parameters)
      if (is.null(nms) || any(nms == "")) {
        stop("`parameters` must be a named list with names representing aperture values.")
      }
      aperture_num <- suppressWarnings(as.numeric(nms))
      if (any(is.na(aperture_num))) {
        stop("All `parameters` names must be coercible to numeric aperture values (f-numbers).")
      }
      if (any(!is.finite(aperture_num)) || any(aperture_num < 0)) {
        stop("Aperture values in `parameters` must be positive and finite.")
      }
      if (any(duplicated(aperture_num))) {
        stop("Duplicated aperture values detected in `parameters`.")
      }
      ok <- vapply(
        parameters,
        function(x) is.numeric(x) && length(x) >= 1,
        logical(1)
      )
      if (!all(ok)) {
        stop("Each element of `parameters` must be a numeric vector of length >= 1.")
      }
    }
    # Cross-validation for flat-field specifications
    if (type == "flat_field_correction") {

      # --- validate model first ---
      .check_flat_field_model(model)

      # --- data-driven mode attempted ---
      if (!is.null(data)) {

        .check_flat_field_data(data)

        if (model != "flat_field_trend_surface")
          stop(
            "`data` requires `model = \"flat_field_trend_surface\"`.",
            call. = FALSE
          )

        if (!is.null(parameters))
          stop(
            "`parameters` must be NULL when `data` is supplied.",
            call. = FALSE
          )

      } else {

        # --- parametric mode attempted ---
        if (model == "flat_field_trend_surface")
          stop(
            "`flat_field_trend_surface` requires `data`.",
            call. = FALSE
          )

        if (is.null(parameters))
          stop(
            "`parameters` must be supplied for parametric flat-field models.",
            call. = FALSE
          )
      }
    }
  }
  if (type == "interpretive_constraint") {
    if (!is.null(offset_value)) {
      if (!is.list(offset_value)) {
        stop("`offset_value` must be a list.")
      }
      nms <- names(offset_value)
      if (is.null(nms) || any(nms == "")) {
        stop("`offset_value` must be a named list with names representing ISO sensitivity values.")
      }
      aperture_num <- suppressWarnings(as.numeric(nms))
      if (any(is.na(aperture_num))) {
        stop("All `offset_value` names must be coercible to numeric ISO sensitivity values.")
      }
      if (any(!is.finite(aperture_num)) || any(aperture_num <= 0)) {
        stop("ISO sensitivity values in `offset_value` must be positive and finite.")
      }
      if (any(duplicated(aperture_num))) {
        stop("Duplicated ISO sensitivity values detected in `offset_value`.")
      }
      ok <- vapply(
        offset_value,
        function(x) is.numeric(x),
        logical(1)
      )
      if (!all(ok)) {
        stop("Each element of `offset_value` must be a numeric vector.")
      }
    }

    .check_cfa_pattern(cfa_pattern)
    .check_spectral_mapping(spectral_mapping, cfa_pattern)
  }

  .check_vector(date, "date", 1)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  x <- list(
    id = id,
    type = type,
    model = model,
    rcaiman_version = rcaiman_version,
    parameters = parameters,
    data = data,
    cfa_pattern = cfa_pattern,
    spectral_mapping = spectral_mapping,
    offset_value = offset_value,
    date = date,
    notes = notes,
    contact_information = contact_information
  )

  class(x) <- "radiometry_spec"
  x
}

#' Add a radiometry spec to an existing geometry spec
#'
#' @description
#' Add a [radiometry_spec] to a [hs_registry_entry] object.
#'
#' @details
#' The behavior and required arguments depend on the value of `type`.
#'
#' When `type = "flat_field_correction"`, the specification must include a valid
#' `model` identifier and associated `parameters` or flat-field `data`. The
#' model is implemented in \pkg{rcaiman} as a function and can be resolved at
#' runtime based on its name. Package version is stored for traceability and
#' reproducibility purposes.
#'
#' When `type = "interpretive_constraint"`, the specification declares
#' constraints or conventions that affect how sensor data must be interpreted.
#' At least one or these arguments must be provided: `cfa_pattern` and
#' `spectral_mapping`. Modeling-related arguments are ignored.
#'
#' @param geometry_spec character vector of length one. Identifier of the geometry
#'   specification to which the radiometry spec will be attached.
#' @param id character vector of length one. Identifier of the radiometry spec.
#'   Must use snake_case (lowercase letters, numbers, and underscores only)
#' @param type character vector of length one. See *Details*.
#' @param model character vector of length one. Identifier of a flat-field model
#'   implemented in \pkg{rcaiman} package as a public function. Required when
#'   `type = "flat_field_correction"`. The value should match the name of a
#'   function. A flat-field model function that:
#'   \itemize{
#'     \item Has a first argument representing the primary input raster,
#'       which must be a single-layer [terra::SpatRaster-class].
#'     \item May declare an argument named `parameters` used to receive model
#'       parameters.
#'     \item Returns a single-layer [terra::SpatRaster-class].
#'     \item Preserves raster geometry (extent, resolution, dimensions,
#'       and CRS) with respect to the first raster argument.
#'     \item Produces strictly positive or `NA` values and is therefore
#'       interoperable with [rcaiman::apply_flat_field()].
#'   }
#' @param rcaiman_version character vector of length one. Version of the
#'   \pkg{rcaiman} package used when the specification was created. If `NULL`,
#'   the current installed version of \pkg{rcaiman} is recorded.
#' @param parameters optional named list of model parameters. Parameters for
#'   `model`. List names must be coercible to numeric values
#'   representing f-numbers (provide `"0"` to indicates that is applicable to
#'   any f-number). This argument is intended for use with
#'   `type = "flat_field_correction"`.
#' @param data optional list. Dense flat-field data apt to build a flat-field
#'   image. Intended for use with `type = "flat_field_correction"`. The expected
#'   structure is:
#'   \itemize{
#'     \item `angles`: data frame with numeric columns `zenith` and
#'     `azimuth` (in degrees), describing the angular sampling positions.
#'     \item `relative_radiance`: named list of data frames. Each name must represent
#'     a positive numeric aperture value (f-number). Each data frame must
#'     contain at least one numeric column with the same number
#'     of rows as `angles`.
#'   }
#'   Angular values must be finite. Radiance values must be finite and
#'   non-negative. This structure is intended for models that reconstruct the
#'   flat-field surface from dense and well-distributed sampling points covering
#'   the field of view. When `data` is supplied, `model` must be
#'   `"flat_field_trend_surface"` and `parameters` must be `NULL`.
#' @param cfa_pattern character matrix of two rows by two columns. Declares the
#'   ordered set of color filter elements used by the sensor. This argument must
#'   be provided when `type = "interpretive_constraint"`. Values should
#'   correspond to semantic color labels (e.g. `"Red"`, `"Green"`, `"Blue"`,
#'   `"Yellow"`, `"Cyan"`, `"Magenta"`), and reflect the CFA pattern as produced
#'   by the sensor.
#' @param spectral_mapping optional named list. Declares how target spectral
#'   bands should be derived from sensor channels. This argument is intended for
#'   use with `type = "interpretive_constraint"`. Each element of the list must
#'   be named according to the target band (e.g. `"Red"`, `"Green"`, `"Blue"`).
#'   The corresponding value must be a character scalar containing an algebraic
#'   expression that defines how the target band is computed from the sensor
#'   channels declared in `cfa_pattern`. Symbols used in the expression must
#'   match channel labels in `cfa_pattern`. Only numeric constants and the
#'   operators `+`, `-`, `*`, `/`, and parentheses are allowed.
#' @param offset_value optional named list of black levels. List names must be coercible
#'   to numeric values representing ISO sensitivity.
#'
#' @inheritParams add_embedded_metadata_sig
#' @inheritParams add_geometry_spec
#'
#' @return
#' A [hs_registry_entry] object with the [radiometry_spec] appended to the selected [geometry_spec].
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#'
#' # Generate the data argument ----------------------------------------------
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
#' # -------------------------------------------------------------------------
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
#' foo
#' summary(foo)
#' }
add_radiometry_spec <- function(
    registry_entry,
    embedded_metadata_sig,
    geometry_spec,
    id,
    type,
    model = NULL,
    rcaiman_version = NULL,
    parameters = NULL,
    data = NULL,
    cfa_pattern = NULL,
    spectral_mapping = NULL,
    offset_value = NULL,
    date = NULL,
    notes = NULL,
    contact_information = NULL
) {

  .check_registry_entry(registry_entry)
  if (is.null(date)) {
    date <- Sys.Date()
  }
  .check_vector(embedded_metadata_sig, "character", 1)
  if (is.null(registry_entry[[embedded_metadata_sig]])) {
    stop(
      sprintf("metadata signature with id '%s' does not exist in this registry entry.",
              embedded_metadata_sig),
      call. = FALSE
    )
  }
  .embedded_metadata_sig <- registry_entry[[embedded_metadata_sig]]
  .check_embedded_metadata_sig(.embedded_metadata_sig)
  .check_vector(geometry_spec, "character", 1)
  if (is.null(.embedded_metadata_sig$geometry[[geometry_spec]])) {
    stop(
      sprintf(
        "Geometry spec with id '%s' does not exists for embedded metadata signature '%s'.",
        geometry_spec, embedded_metadata_sig
      ),
      call. = FALSE
    )
  }
  geometry <- .embedded_metadata_sig$geometry[[geometry_spec]]
  .check_geometry_spec(geometry)
  .check_vector(id, "character", 1)
  if (!is.null(geometry$radiometry[[id]])) {
    stop(
      sprintf(
        "Radiometry spec with id '%s' already exists for geometry '%s'.",
        id, geometry_spec
      ),
      call. = FALSE
    )
  }

  radiometry_spec <- .new_radiometry_spec(
    id = id,
    type = type,
    model = model,
    rcaiman_version = rcaiman_version,
    parameters = parameters,
    data = data,
    cfa_pattern = cfa_pattern,
    spectral_mapping = spectral_mapping,
    offset_value = offset_value,
    date = date,
    notes = notes,
    contact_information = contact_information
  )

  registry_entry[[embedded_metadata_sig]]$geometry[[geometry_spec]]$radiometry[[id]] <- radiometry_spec
  registry_entry
}
