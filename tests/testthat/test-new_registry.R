test_that("`new_registry_entry()` returns a `hs_registry_entry`", {
  expect_true(inherits(new_registry_entry("test", device = "test"), "hs_registry_entry"))
})


geometry_test_registry <- function() {
  registry_entry <- new_registry_entry("test", device = "test")
  add_embedded_metadata_sig(
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
}


test_that("geometry specs store the snapped zenith coordinate", {
  registry_entry <- add_geometry_spec(
    geometry_test_registry(),
    embedded_metadata_sig = "test_metadata",
    id = "test_geometry",
    model = "radial_projection",
    parameters = equidistant_coef,
    snapped_zenith_coord = c(10, 10),
    horizon_radius = 3,
    is_horizon_circle_clipped = FALSE
  )
  geometry_spec <- get_geometry_spec(
    registry_entry,
    "test_metadata",
    "test_geometry"
  )

  expect_identical(geometry_spec$snapped_zenith_coord, c(10, 10))
  expect_identical(geometry_spec$horizon_radius, 3)
  expect_null(geometry_spec$zenith_col_row)
})


test_that("geometry specs require snapped coordinates and an integer radius", {
  common_arguments <- list(
    registry_entry = geometry_test_registry(),
    embedded_metadata_sig = "test_metadata",
    id = "test_geometry",
    model = "radial_projection",
    parameters = equidistant_coef,
    horizon_radius = 3,
    is_horizon_circle_clipped = FALSE
  )

  expect_error(
    do.call(
      add_geometry_spec,
      c(common_arguments, list(snapped_zenith_coord = c(10.5, 10)))
    ),
    "integer-like"
  )
  common_arguments$horizon_radius <- 3.5
  expect_error(
    do.call(
      add_geometry_spec,
      c(common_arguments, list(snapped_zenith_coord = c(10, 10)))
    ),
    "integer-like"
  )
})


test_that("geometry specs dispatch anisotropic parameter validation to core", {
  parameters <- list(
    zen_coef = c(0.8, 0.1, -0.02),
    azi_sin_coef = c(1, 0),
    azi_cos_coef = c(0, 1),
    atan_scale = 0.45
  )
  registry_entry <- add_geometry_spec(
    geometry_test_registry(),
    embedded_metadata_sig = "test_metadata",
    id = "anisotropic_geometry",
    model = "anisotropic_projection",
    parameters = parameters,
    snapped_zenith_coord = c(10, 10),
    horizon_radius = 3,
    is_horizon_circle_clipped = FALSE
  )
  geometry_spec <- get_geometry_spec(
    registry_entry,
    "test_metadata",
    "anisotropic_geometry"
  )

  expect_identical(geometry_spec$model, "anisotropic_projection")
  expect_identical(geometry_spec$parameters, parameters)

  invalid_parameters <- parameters
  invalid_parameters$atan_scale <- NULL
  expect_error(
    add_geometry_spec(
      geometry_test_registry(),
      embedded_metadata_sig = "test_metadata",
      id = "invalid_anisotropic_geometry",
      model = "anisotropic_projection",
      parameters = invalid_parameters,
      snapped_zenith_coord = c(10, 10),
      horizon_radius = 3,
      is_horizon_circle_clipped = FALSE
    ),
    "atan_scale"
  )
})


test_that("geometry calibration interfaces require continuous image columns", {
  old_columns <- data.frame(
    col = c(1, 2, 3),
    row = c(1, 2, 1)
  )

  expect_error(calc_zenith_coord(old_columns), "x, y_down")
  expect_error(calibrate_geometry(list(old_columns)), "x, y_down")
})
