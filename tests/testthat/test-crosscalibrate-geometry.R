# Preserve the intended historical radial algorithm within the supported
# package workflow. These tests guard against incidental algorithmic changes.
radial_crosscalibration_fixture <- function() {
  theta <- c(0.1, 0.3, 0.55, 0.8, 1.05, 1.3, 1.5)
  image_angle <- c(0, 0.4, 1.1, 2, -2.4, -1.2, -0.3)
  diameter_cal <- 200
  zenith_coord_cal <- c(110, 120)
  zenith_coord_uncal <- c(90, 95)

  calibrated_radius <- calc_relative_radius(
    theta,
    equidistant_coef,
    units = "radian"
  ) * diameter_cal / 2
  uncalibrated_radius <-
    40 * theta + 5 * theta^2 + 2 * theta^3

  radial_coord <- function(radius, image_angle, zenith_coord) {
    data.frame(
      x = zenith_coord[1] + radius * cos(image_angle),
      y_down = zenith_coord[2] + radius * sin(image_angle)
    )
  }

  list(
    theta = theta,
    px = uncalibrated_radius,
    pts_cal = radial_coord(
      calibrated_radius,
      image_angle,
      zenith_coord_cal
    ),
    pts_uncal = radial_coord(
      uncalibrated_radius,
      image_angle,
      zenith_coord_uncal
    ),
    zenith_coord_cal = zenith_coord_cal,
    zenith_coord_uncal = zenith_coord_uncal,
    diameter_cal = diameter_cal
  )
}


test_that("radial cross-calibration preserves its polynomial contract", {
  fixture <- radial_crosscalibration_fixture()
  observed <- crosscalibrate_geometry(
    fixture$pts_uncal,
    fixture$pts_cal,
    model = "radial_projection",
    zenith_coord_uncal = fixture$zenith_coord_uncal,
    zenith_coord_cal = fixture$zenith_coord_cal,
    diameter_cal = fixture$diameter_cal,
    projection_coef = equidistant_coef,
    degree = 3
  )
  expected_horizon_radius <-
    40 * (pi / 2) + 5 * (pi / 2)^2 + 2 * (pi / 2)^3

  expect_named(
    observed,
    c("ds", "model", "horizon_radius", "projection_coef")
  )
  expect_equal(observed$ds, data.frame(theta = fixture$theta, px = fixture$px))
  expect_s3_class(observed$model, "lm")
  expect_equal(attr(stats::terms(observed$model), "intercept"), 0)
  expect_equal(unname(stats::coef(observed$model)), c(40, 5, 2))
  expect_equal(observed$horizon_radius, expected_horizon_radius)
  expect_equal(
    observed$projection_coef,
    c(40, 5, 2) / expected_horizon_radius
  )
})


test_that("radial cross-calibration consumes coord and preserves pairing", {
  fixture <- radial_crosscalibration_fixture()
  fixture$pts_uncal$id <- rev(seq_len(nrow(fixture$pts_uncal)))
  fixture$pts_cal$note <- letters[seq_len(nrow(fixture$pts_cal))]

  observed <- crosscalibrate_geometry(
    fixture$pts_uncal,
    fixture$pts_cal,
    model = "radial_projection",
    zenith_coord_uncal = fixture$zenith_coord_uncal,
    zenith_coord_cal = fixture$zenith_coord_cal,
    diameter_cal = fixture$diameter_cal,
    projection_coef = equidistant_coef
  )

  expect_equal(observed$ds$theta, fixture$theta)
  expect_equal(observed$ds$px, fixture$px)

  old_representation <- data.frame(
    col = fixture$pts_uncal$x,
    row = fixture$pts_uncal$y_down
  )
  expect_error(
    crosscalibrate_geometry(
      old_representation,
      fixture$pts_cal,
      model = "radial_projection",
      zenith_coord_uncal = fixture$zenith_coord_uncal,
      zenith_coord_cal = fixture$zenith_coord_cal,
      diameter_cal = fixture$diameter_cal,
      projection_coef = equidistant_coef
    ),
    "x, y_down"
  )

  expect_error(
    crosscalibrate_geometry(
      fixture$pts_uncal[-1, ],
      fixture$pts_cal,
      model = "radial_projection",
      zenith_coord_uncal = fixture$zenith_coord_uncal,
      zenith_coord_cal = fixture$zenith_coord_cal,
      diameter_cal = fixture$diameter_cal,
      projection_coef = equidistant_coef
    ),
    "same number of rows"
  )
})


test_that("cross-calibration requires a supported model identifier", {
  fixture <- radial_crosscalibration_fixture()
  args <- list(
    pts_uncal = fixture$pts_uncal,
    pts_cal = fixture$pts_cal,
    zenith_coord_uncal = fixture$zenith_coord_uncal,
    zenith_coord_cal = fixture$zenith_coord_cal,
    diameter_cal = fixture$diameter_cal,
    projection_coef = equidistant_coef
  )

  expect_error(
    do.call(crosscalibrate_geometry, args),
    "`model` must be supplied"
  )

  args$model <- "direction_model"
  expect_error(
    do.call(crosscalibrate_geometry, args),
    "radial_projection"
  )
})


anisotropic_crosscalibration_fixture <- function() {
  pts_uncal <- data.frame(
    x = c(1, 2, 3, 5, 7, 6, 8, 2, 1, 5, 7, 4),
    y_down = c(1, 5, 2, 4, 1, 5, 3, 2, 4, 1, 6, 5),
    id = letters[1:12]
  )
  width_uncal <- 8
  height_uncal <- 6
  atan_scale <- 0.45
  zen_coef <- c(0.8, 0.1, -0.02)
  angle_offset <- 0.35

  prepared <- add_image_polar(pts_uncal, width_uncal, height_uncal)
  atan_radius <- atan(prepared$relative_radius / atan_scale)
  zen <-
    zen_coef[1] * atan_radius +
    zen_coef[2] * atan_radius^2 +
    zen_coef[3] * atan_radius^3
  azi <- (prepared$image_angle + angle_offset) %% (2 * pi)

  pts_cal <- data.frame(
    x = pts_uncal$x + 10,
    y_down = pts_uncal$y_down + 20,
    zen = zen,
    azi = azi
  )

  list(
    pts_uncal = pts_uncal,
    pts_cal = pts_cal,
    width_uncal = width_uncal,
    height_uncal = height_uncal,
    atan_scale = atan_scale,
    zen_coef = zen_coef,
    azi_sin_coef = c(cos(angle_offset), sin(angle_offset)),
    azi_cos_coef = c(-sin(angle_offset), cos(angle_offset))
  )
}


test_that("anisotropic cross-calibration returns the core parameter contract", {
  fixture <- anisotropic_crosscalibration_fixture()
  observed <- crosscalibrate_geometry(
    fixture$pts_uncal,
    fixture$pts_cal,
    model = "anisotropic_projection",
    degree = 3,
    width_uncal = fixture$width_uncal,
    height_uncal = fixture$height_uncal,
    atan_scale = fixture$atan_scale
  )

  expect_named(observed, c("ds", "model", "parameters"))
  expect_named(observed$model, c("zen", "azi_sin", "azi_cos"))
  expect_true(all(vapply(observed$model, inherits, logical(1), "lm")))
  expect_identical(observed$ds$id, fixture$pts_uncal$id)
  expect_equal(observed$ds$zen, fixture$pts_cal$zen)
  expect_equal(observed$ds$azi, fixture$pts_cal$azi)
  expect_equal(observed$parameters$zen_coef, fixture$zen_coef)
  expect_equal(observed$parameters$azi_sin_coef, fixture$azi_sin_coef)
  expect_equal(observed$parameters$azi_cos_coef, fixture$azi_cos_coef)
  expect_identical(observed$parameters$atan_scale, fixture$atan_scale)
  expect_true(.assert_anisotropic_projection_parameters(observed$parameters))

  prediction_coord <- add_image_polar(
    fixture$pts_uncal[c("x", "y_down", "id")],
    fixture$width_uncal,
    fixture$height_uncal
  )
  predicted <- add_directions_from_model(
    prediction_coord,
    observed$parameters
  )
  expect_equal(predicted$zen, fixture$pts_cal$zen)
  expect_equal(predicted$azi, fixture$pts_cal$azi)
})


test_that("anisotropic cross-calibration validates its branch contract", {
  fixture <- anisotropic_crosscalibration_fixture()
  args <- list(
    pts_uncal = fixture$pts_uncal,
    pts_cal = fixture$pts_cal,
    model = "anisotropic_projection",
    degree = 3,
    width_uncal = fixture$width_uncal,
    height_uncal = fixture$height_uncal,
    atan_scale = fixture$atan_scale
  )

  args_without_azi <- args
  args_without_azi$pts_cal$azi <- NULL
  expect_error(
    do.call(crosscalibrate_geometry, args_without_azi),
    "azi"
  )

  args_without_width <- args
  args_without_width$width_uncal <- NULL
  expect_error(
    do.call(crosscalibrate_geometry, args_without_width),
    "width"
  )

  args_with_stale_geometry <- args
  args_with_stale_geometry$pts_uncal$relative_radius <- 0
  expect_error(
    do.call(crosscalibrate_geometry, args_with_stale_geometry),
    "reserved columns"
  )
})
