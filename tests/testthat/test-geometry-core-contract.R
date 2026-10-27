geometry_contract_entry <- function() {
  entry <- new_registry_entry("test", device = "Test device")
  entry <- add_signature(
    entry, "raw", dim = c(8, 6),
    validated_with_libraw = NA_character_,
    validated_with_exiftool = NA_character_,
    rules = list(model = list(group = "IFD0", tag = "Model", label = "Model",
                             expected = "Test device"))
  )
  add_band_spec(entry, "raw", "native")
}

geometry_contract_spec <- function(...) {
  args <- utils::modifyList(list(
    registry_entry = geometry_contract_entry(),
    signature = "raw", band_spec = "native", id = "geometry",
    model = "radial_projection", center_of_vision = c(4, 3),
    parameters = normalize_projection_coef(equidistant_coef, 60),
    normalization_radius = 6.25, normalization_angle = 60
  ), list(...), keep.null = TRUE)
  entry <- do.call(add_geometry_spec, args)
  get_geometry_spec(entry, "raw", "native", "geometry")
}

test_that("registry geometry passes directly through the oriented core workflow", {
  spec <- geometry_contract_spec(max_observed_off_axis_angle = 45)
  plain <- unclass(spec)
  original <- spec
  ig <- set_internal_geometry(c(8, 6), spec)
  expect_equal(terra::values(ig),
                 terra::values(set_internal_geometry(c(8, 6), plain)))
  d <- direction_image(ig, sensor_rotation = 90, azimuth_rotation = 70,
                        elevation = 32.5)
  indices <- expand.grid(col = 1:8, row = 1:6)
  indices <- rem_unselected(indices, !is.na(ig$off_axis) & !is.na(ig$internal_azimuth))
  points <- add_directions(indices, d = d)
  projected <- reproject_points(points, spec, sensor_rotation = 90,
                                 azimuth_rotation = 70, elevation = 32.5)
  xy <- terra::xyFromCell(ig, seq_len(terra::ncell(ig)))
  expect_equal(projected$x, xy[, 1], tolerance = 1e-8)
  expect_equal(projected$y_down, 6 - xy[, 2], tolerance = 1e-8)
  r <- ig[[1]]
  terra::values(r) <- seq_len(terra::ncell(r))
  out <- reproject_image(
    r, d, c(8, 6), spec, sensor_rotation = 90, azimuth_rotation = 70,
    elevation = 32.5, search_radius = 0.1, parallel = FALSE
  )
  expect_equal(terra::values(out), terra::values(r))
  expect_identical(spec, original)
  expect_identical(spec$normalization_radius, 6.25)
  expect_identical(spec$normalization_angle, 60)
  expect_identical(spec$max_off_axis_angle, 90)
})

test_that("observed calibration range does not alter normalization or clipping", {
  spec <- geometry_contract_spec(max_observed_off_axis_angle = 30)
  above <- geometry_contract_spec(max_observed_off_axis_angle = 120)
  expect_identical(above$max_observed_off_axis_angle, 120)
  expect_equal(terra::values(set_internal_geometry(c(8, 6), spec)),
                 terra::values(set_internal_geometry(c(8, 6), above)))
  points <- data.frame(zenith = c(60, 95) * pi / 180, azimuth = c(0, 0))
  out <- reproject_points(points, spec)
  expect_equal(out$y_down[1], 3 - 6.25)
  expect_true(is.na(out$y_down[2]))
  expect_null(geometry_contract_spec()$max_observed_off_axis_angle)
  for (observed in list(-1, 181, Inf, NA_real_, c(30, 60))) {
    expect_error(geometry_contract_spec(max_observed_off_axis_angle = observed),
                   "max_observed_off_axis_angle")
  }
})

test_that("geometry no longer requires a contained reference circle or center", {
  spec <- geometry_contract_spec(center_of_vision = c(0, -2),
                                  normalization_radius = 100.5)
  expect_identical(spec$center_of_vision, c(0, -2))
  expect_true(all(is.finite(terra::values(set_internal_geometry(c(8, 6), spec)))))
  expect_false(any(c("snapped_zenith_coord", "horizon_radius",
                     "is_horizon_circle_clipped", "max_zenith_angle") %in%
                     names(spec)))
  expect_error(geometry_contract_spec(is_horizon_circle_clipped = FALSE),
                 "unused argument")
})

test_that("radial geometry registration enforces the core contract unchanged", {
  expect_error(geometry_contract_spec(normalization_angle = NULL),
                 "normalization_angle")
  expect_error(geometry_contract_spec(normalization_radius = NULL),
                 "normalization_radius")
  expect_error(geometry_contract_spec(normalization_radius = 0),
                 "normalization_radius")
  expect_error(geometry_contract_spec(center_of_vision = c(4.5, 3)),
                 "center_of_vision")
  expect_error(geometry_contract_spec(parameters = equidistant_coef),
                 "must equal one")
  expect_error(geometry_contract_spec(max_off_axis_angle = 181),
                 "180")
  # A polynomial normalized at 30 degrees but decreasing later in the interval.
  expect_error(geometry_contract_spec(
    parameters = normalize_projection_coef(c(1, -1), 30),
    normalization_angle = 30
  ), "strictly increasing")
  spec <- geometry_contract_spec(max_off_axis_angle = 120)
  expect_identical(spec$max_off_axis_angle, 120)
})

test_that("anisotropic specs use internal parameters without radial requirements", {
  parameters <- list(off_axis_coef = c(0.4, 0.05),
                      sin_internal_azimuth_coef = c(0, 1),
                      cos_internal_azimuth_coef = c(-1, 0),
                      atan_scale = 0.6)
  spec <- geometry_contract_spec(
    model = "anisotropic_projection", parameters = parameters,
    normalization_radius = NULL, normalization_angle = NULL,
    max_observed_off_axis_angle = 70
  )
  expect_identical(spec$parameters, parameters)
  expect_null(spec$normalization_radius)
  expect_null(spec$normalization_angle)
  ig <- set_internal_geometry(c(8, 6), spec)
  expect_true(all(is.finite(terra::values(ig))))
  expect_equal(terra::values(ig), terra::values(set_internal_geometry(
    c(8, 6), list(model = spec$model, center_of_vision = spec$center_of_vision,
                 parameters = parameters)
  )))
  bad <- parameters
  names(bad)[1] <- "zen_coef"
  expect_error(geometry_contract_spec(model = "anisotropic_projection",
                                      parameters = bad), "off_axis_coef")
})
