.new_radiometry_spec <- function(
    id,
    model = NULL,
    rcaiman_version = NULL,
    parameters = NULL,
    date = NULL,
    notes = NULL,
    contact_information = NULL
) {

  .assert_id(id)

  .check_flat_field_model(model)
  if (is.null(rcaiman_version)) {
    rcaiman_version <- as.character(utils::packageVersion("rcaiman"))
  }
  .check_vector(rcaiman_version, "character", 1)
  .check_radiometry_parameters(parameters, model)

  .check_vector(date, "date", 1)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  x <- list(
    id = id,
    model = model,
    rcaiman_version = rcaiman_version,
    parameters = parameters,
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
#' Add a [radiometry_spec] declaring a flat-field correction model to a
#' [hs_registry_entry] object.
#'
#' @details
#' The specification must include a valid
#' `model` identifier. Fitted models also require `parameters` organized by
#' aperture and then band. Angular models are evaluated by [rcaiman::calc_relative_response()]
#' and [rcaiman::flat_field_image()]. Package version is stored for traceability and
#' reproducibility purposes.
#'
#' The empirical model `lang2010` contains its coefficients in
#' the core implementation and describes the blue band reported in the publication.
#' Register it with `parameters = NULL` and supply
#' the acquisition aperture when evaluating it with the off-axis angles.
#' Registering this model declares its applicability to the selected system;
#' it does not represent a new fit to calibration observations.
#'
#' @param geometry_spec character vector of length one. Identifier of the geometry
#'   specification to which the radiometry spec will be attached.
#' @param id character vector of length one. Identifier of the radiometry spec.
#'   Must use snake_case (lowercase letters, numbers, and underscores only)
#' @param model character vector of length one. Angular model identifier for
#'   [rcaiman::flat_field_image()]: `"simple_polynomial"`,
#'   `"free_form_polynomial"`, or `"lang2010"`. Alternatively,
#'   `"trend_surface_image"` selects the general surface
#'   evaluator [rcaiman::trend_surface_image()] to model relative response.
#'   See the "Radiometric preprocessing" section in
#'   [rcaiman::trend_surface_image()] for the calibration and application
#'   workflow. Model selection does not guarantee positive predictions;
#'   [rcaiman::apply_flat_field()] checks the evaluated correction.
#' @param rcaiman_version character vector of length one. Version of the
#'   \pkg{rcaiman} package used when the specification was created. If `NULL`,
#'   the current installed version of \pkg{rcaiman} is recorded.
#' @param parameters `NULL` for the blue-band model `lang2010`; otherwise a
#'   named list organized as aperture, then band, then fitted parameters.
#'   Aperture names must be finite non-negative numeric values without numeric
#'   duplicates; `"0"` declares applicability to any aperture. Each aperture
#'   contains a non-empty list with unique nonempty band names, for example
#'   `list("5.6" = list(Cyan = fitted_parameters))`. Use the band identifiers
#'   of the data being corrected, whether source or spectrally mapped bands.
#'   Only calibrated bands need to be listed; omission does not imply that
#'   another band's calibration applies. Different apertures may list different
#'   bands. For `trend_surface_image`, each leaf is the plain list from
#'   [rcaiman::fit_trend_surface()] (`np`, `beta`, `rx`, `ry`); for polynomial
#'   models, each leaf is a non-empty numeric vector. Pass the selected leaf
#'   unchanged to the core evaluator. Do not supply full fitted model objects
#'   or calibration observations.
#' @inheritParams add_signature
#' @inheritParams add_geometry_spec
#'
#' @return
#' A [hs_registry_entry] object with the [radiometry_spec] appended to the selected [geometry_spec].
#'
#' @export
#'
#' @examples
#' entry <- new_registry_entry("example", device = "Example device")
#' entry <- add_signature(
#'   entry, "raw",
#'   dim = c(20, 12),
#'   validated_with_libraw = NA,
#'   validated_with_exiftool = NA,
#'   rules = list(model = list(
#'     group = "IFD0",
#'     tag = "Model",
#'     label = "Model",
#'     expected = "Example device"
#'   ))
#' )
#' entry <- add_band_spec(entry, "raw", "bands",
#'                        cfa_pattern = matrix(letters[1:4], nrow = 2))
#' entry <- add_geometry_spec(
#'   entry,
#'   "raw",
#'   "bands",
#'   "radial",
#'   model = "radial_projection",
#'   parameters = equidistant_coef,
#'   center_of_vision = c(10, 6),
#'   normalization_radius = 10,
#'   normalization_angle = 90
#' )
#'
#' # Synthetic normalized response for one band at aperture f/5.
#' samples <- expand.grid(x = seq(0.5, 19.5, length.out = 8),
#'                        y_down = seq(0.5, 11.5, length.out = 8))
#' samples$relative_response <- 1 - 0.001 * (samples$x - 10)^2 -
#'   0.002 * (samples$y_down - 6)^2
#'
#' fitted_parameters <- fit_trend_surface(
#'   samples, col_id = "relative_response", np = 2
#' )
#'
#' entry <- add_radiometry_spec(
#'   entry,
#'   "raw",
#'   "bands",
#'   "radial",
#'   "spatial_response",
#'   model = "trend_surface_image",
#'   parameters = list("5" = list(Blue = fitted_parameters))
#' )
#'
#' radiometry <- get_radiometry_spec(
#'   entry, "raw", "bands", "radial", "spatial_response"
#' )
#'
#' r <- set_rcaiman_geometry(terra::rast(ncols = 20, nrows = 12))
#' ff <- trend_surface_image(r, radiometry$parameters[["5"]]$Blue)
#' ff
#' summary(entry)
#'
#' # An empirical model has no fitted parameters to register.
#' entry <- add_radiometry_spec(
#'   entry, "raw", "bands", "radial", "lang", model = "lang2010"
#' )
#' lang <- get_radiometry_spec(entry, "raw", "bands", "radial", "lang")
#' geometry <- get_geometry_spec(entry, "raw", "bands", "radial")
#' ig <- set_internal_geometry(entry$raw$dim, geometry)
#' ff <- flat_field_image(ig$off_axis, lang$model, aperture = 5)
#' ff
#'
#'
#' \dontrun{
#'   my_entry <- read_registry_snapshot(system.file(
#'     "external/my_entry_v1.yaml", package = "rcaiman.registry"
#'   ))
#'
#'   my_entry <- add_radiometry_spec(
#'     my_entry,
#'     signature = "raw",
#'     band_spec = "cygm",
#'     geometry_spec = "simple_method",
#'     id = "simple_method",
#'     model = "simple_polynomial",
#'     parameters = list("0" = list("Cyan" = c(0.0638 - 0.101))),
#'     notes = "Documented in 10.1016/j.agrformet.2024.110020" ,
#'     contact_information = "gastonmaurodiaz@gmail.com"
#'   )
#'
#'   summary(my_entry)
#'   simple <- get_radiometry_spec(my_entry, "raw", "cygm", "simple_method", "simple_method")
#'   ff <- flat_field_image(ig$off_axis, simple$model, parameters = simple$parameters$`0`$Cyan)
#'   ff
#'   plot(ff)
#' }
add_radiometry_spec <- function(
    registry_entry,
    signature,
    band_spec,
    geometry_spec,
    id,
    model = NULL,
    rcaiman_version = NULL,
    parameters = NULL,
    date = NULL,
    notes = NULL,
    contact_information = NULL
) {

  .check_registry_entry(registry_entry)
  if (is.null(date)) {
    date <- Sys.Date()
  }
  geometry <- get_geometry_spec(
    registry_entry, signature, band_spec, geometry_spec
  )
  .check_geometry_spec(geometry)
  .check_vector(id, "character", 1)
  if (!is.null(geometry$radiometry[[id]])) {
    stop(
      sprintf(
        "Radiometry spec '%s' already exists under geometry '%s', band '%s', metadata '%s'.",
        id, geometry_spec, band_spec, signature
      ),
      call. = FALSE
    )
  }

  radiometry_spec <- .new_radiometry_spec(
    id = id,
    model = model,
    rcaiman_version = rcaiman_version,
    parameters = parameters,
    date = date,
    notes = notes,
    contact_information = contact_information
  )

  registry_entry[[signature]]$bands[[band_spec]]$geometry[[geometry_spec]]$radiometry[[id]] <- radiometry_spec
  registry_entry
}
