test_that("`new_registry_entry()` returns a `hs_registry_entry`", {
  expect_true(inherits(new_registry_entry("test", device = "test"), "hs_registry_entry"))
})


geometry_test_registry <- function() {
  registry_entry <- new_registry_entry("test", device = "test")
  registry_entry <- add_signature(
    registry_entry,
    id = "test_metadata",
    rules = list(
      model = list(
        group = "IFD0",
        tag = "Model",
        label = "Model",
        expected = "test"
      )
    ),
    dim = c(20, 20),
    validated_with_libraw = NA_character_,
    validated_with_exiftool = NA_character_
  )
  add_band_spec(registry_entry, "test_metadata", "test_bands")
}


test_that("geometry specs store the internal reference and omit legacy fields", {
  registry_entry <- add_geometry_spec(
    geometry_test_registry(),
    signature = "test_metadata",
    band_spec = "test_bands",
    id = "test_geometry",
    model = "radial_projection",
    parameters = equidistant_coef,
    center_of_vision = c(10, 10),
    normalization_radius = 3,
    normalization_angle = 90
  )
  geometry_spec <- get_geometry_spec(
    registry_entry,
    "test_metadata",
    "test_bands",
    "test_geometry"
  )

  expect_identical(geometry_spec$center_of_vision, c(10, 10))
  expect_identical(geometry_spec$normalization_radius, 3)
  expect_false(any(c("zenith_col_row", "snapped_zenith_coord", "horizon_radius",
                     "is_horizon_circle_clipped", "max_zenith_angle") %in%
                     names(geometry_spec)))
  expect_identical(geometry_spec$normalization_angle, 90)
  expect_identical(geometry_spec$max_off_axis_angle, 90)
})


test_that("geometry specs require a snapped center and allow a fractional radius", {
  common_arguments <- list(
    registry_entry = geometry_test_registry(),
    signature = "test_metadata",
    band_spec = "test_bands",
    id = "test_geometry",
    model = "radial_projection",
    parameters = equidistant_coef,
    normalization_radius = 3,
    normalization_angle = 90
  )

  expect_error(
    do.call(
      add_geometry_spec,
      c(common_arguments, list(center_of_vision = c(10.5, 10)))
    ),
    "integer-like"
  )
  common_arguments$normalization_radius <- 3.5
  entry <- do.call(
    add_geometry_spec,
    c(common_arguments, list(center_of_vision = c(10, 10)))
  )
  expect_identical(
    get_geometry_spec(entry, "test_metadata", "test_bands",
                      "test_geometry")$normalization_radius, 3.5
  )
})


test_that("geometry specs dispatch anisotropic parameter validation to core", {
  parameters <- list(
    off_axis_coef = c(0.8, 0.1, -0.02),
    sin_internal_azimuth_coef = c(1, 0),
    cos_internal_azimuth_coef = c(0, 1),
    atan_scale = 0.45
  )
  registry_entry <- add_geometry_spec(
    geometry_test_registry(),
    signature = "test_metadata",
    band_spec = "test_bands",
    id = "anisotropic_geometry",
    model = "anisotropic_projection",
    parameters = parameters,
    center_of_vision = c(10, 10)
  )
  geometry_spec <- get_geometry_spec(
    registry_entry,
    "test_metadata",
    "test_bands",
    "anisotropic_geometry"
  )

  expect_identical(geometry_spec$model, "anisotropic_projection")
  expect_identical(geometry_spec$parameters, parameters)

  invalid_parameters <- parameters
  invalid_parameters$atan_scale <- NULL
  expect_error(
    add_geometry_spec(
      geometry_test_registry(),
      signature = "test_metadata",
      band_spec = "test_bands",
      id = "invalid_anisotropic_geometry",
      model = "anisotropic_projection",
      parameters = invalid_parameters,
      center_of_vision = c(10, 10)
    ),
    "atan_scale"
  )
})


test_that("geometry calibration interfaces require continuous image columns", {
  old_columns <- data.frame(
    col = c(1, 2, 3),
    row = c(1, 2, 1)
  )

  expect_error(calc_center_of_vision(old_columns), "x, y_down")
  expect_error(calibrate_geometry(list(old_columns)), "x, y_down")
})
