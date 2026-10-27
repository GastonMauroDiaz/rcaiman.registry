radial_crosscalibration_fixture <- function(reference_model = "radial_projection") {
  geometry <- list(model = reference_model, center_of_vision = c(97, 72),
                   parameters = 2 / pi, normalization_radius = 80,
                   normalization_angle = 90, max_off_axis_angle = 150)
  if (reference_model == "anisotropic_projection") {
    geometry$parameters <- list(off_axis_coef = c(1.9, .1),
      sin_internal_azimuth_coef = c(0, 1),
      cos_internal_azimuth_coef = c(-1, 0), atan_scale = .6)
  }
  ig <- set_internal_geometry(c(200, 160), geometry)
  # Continuous reference coordinates are explicitly sampled on its pixel grid.
  reference_coord <- data.frame(x = c(97.2, 112.7, 51.3, 160.4, 10.1, 180.9, 40.6),
                                y_down = c(72.7, 61.2, 40.9, 120.3, 10.8, 151.1, 140.4))
  indices <- indices_from_coord(reference_coord$x, reference_coord$y_down)
  directions <- add_directions(indices, internal_geometry = ig)
  center <- c(170, 140)
  a <- directions$off_axis
  radius <- 40 * a + 5 * a^2 + 2 * a^3
  # Corresponding rays in a known target projection, before pixel sampling.
  pts_uncal <- data.frame(x = center[1] + radius * sin(directions$internal_azimuth),
    y_down = center[2] - radius * cos(directions$internal_azimuth),
    id = letters[seq_along(radius)])
  list(pts_uncal = pts_uncal, pts_cal = directions, center = center,
       radius = radius, reference_coord = reference_coord)
}

radial_crosscalibration_entry <- function(fit) {
  entry <- new_registry_entry("crosscalibration", device = "Test device")
  entry <- add_signature(entry, "raw", dim = c(360, 300),
    validated_with_libraw = NA_character_, validated_with_exiftool = NA_character_,
    rules = list(model = list(group = "IFD0", tag = "Model", label = "Model",
                              expected = "Test device")))
  entry <- add_band_spec(entry, "raw", "bands")
  add_geometry_spec(entry, "raw", "bands", "crosscalibration",
    model = "radial_projection", parameters = fit$projection_coef,
    center_of_vision = fit$center_of_vision,
    normalization_radius = fit$normalization_radius,
    normalization_angle = fit$normalization_angle, max_off_axis_angle = 150)
}

test_that("radial cross-calibration transfers either reference into usable geometry", {
  for (reference in c("radial_projection", "anisotropic_projection")) {
    f <- radial_crosscalibration_fixture(reference)
    before <- f
    fit <- crosscalibrate_geometry(f$pts_uncal, f$pts_cal, "radial_projection",
      center_of_vision = f$center, normalization_angle = 60)
    expect_named(fit, c("ds", "model", "normalization_radius",
                       "normalization_angle", "projection_coef", "center_of_vision"))
    expect_equal(f, before)
    expect_identical(fit$ds[names(f$pts_uncal)], f$pts_uncal)
    expect_identical(fit$ds$off_axis, f$pts_cal$off_axis)
    expect_equal(fit$ds$radius, f$radius)
    expect_identical(fit$center_of_vision, f$center)
    expect_s3_class(fit$model, "lm")
    expect_equal(attr(stats::terms(fit$model), "intercept"), 0)
    expect_equal(unname(stats::coef(fit$model)), c(40, 5, 2))
    expect_equal(unname(predict(fit$model, f$pts_cal)), f$radius)
    expect_equal(unname(residuals(fit$model)), rep(0, nrow(f$pts_uncal)))
    reference_radius <- 40 * (pi / 3) + 5 * (pi / 3)^2 + 2 * (pi / 3)^3
    expect_equal(fit$normalization_radius, reference_radius)
    expect_equal(fit$projection_coef, c(40, 5, 2) / reference_radius)
    expect_equal(calc_relative_radius(60, fit$projection_coef), 1)

    entry <- radial_crosscalibration_entry(fit)
    spec <- get_geometry_spec(entry, "raw", "bands", "crosscalibration")
    expect_null(spec$max_observed_off_axis_angle)
    ig <- set_internal_geometry(entry$raw$dim, spec)
    known <- list(model = "radial_projection", center_of_vision = f$center,
      parameters = c(40, 5, 2) / reference_radius,
      normalization_radius = reference_radius, normalization_angle = 60,
      max_off_axis_angle = 150)
    expect_equal(values(ig), values(set_internal_geometry(c(360, 300), known)),
                 tolerance = 1e-8)
    # Verify directions at selected target cells, after the continuous fit.
    target_indices <- indices_from_coord(f$pts_uncal$x, f$pts_uncal$y_down)
    target_indices <- rem_unselected(target_indices, !is.na(ig$off_axis) & !is.na(ig$internal_azimuth))
    directions <- add_directions(target_indices, internal_geometry = ig)
    cells <- coord_from_indices(target_indices$col, target_indices$row)
    measured_radius <- sqrt((cells$x - f$center[1])^2 + (cells$y_down - f$center[2])^2)
    expect_equal(calc_relative_radius(directions$off_axis, fit$projection_coef,
                                      units = "radian") * fit$normalization_radius,
                 measured_radius, tolerance = 1e-8)
  }
})

test_that("normalization changes the reference without changing the fitted pixels", {
  f <- radial_crosscalibration_fixture()
  expect_true(any(f$pts_cal$off_axis > pi / 2))
  fit60 <- crosscalibrate_geometry(f$pts_uncal, f$pts_cal, "radial_projection",
                                   f$center, normalization_angle = 60)
  fit90 <- crosscalibrate_geometry(f$pts_uncal, f$pts_cal, "radial_projection", f$center)
  expect_identical(fit90$normalization_angle, 90)
  for (fit in list(fit60, fit90)) {
    expect_equal(calc_relative_radius(f$pts_cal$off_axis, fit$projection_coef,
                                      units = "radian") * fit$normalization_radius,
                 f$radius)
  }
  expect_equal(coef(fit60$model), coef(fit90$model))
  expect_equal(values(set_internal_geometry(c(360, 300), get_geometry_spec(
    radial_crosscalibration_entry(fit60), "raw", "bands", "crosscalibration"))),
    values(set_internal_geometry(c(360, 300), get_geometry_spec(
    radial_crosscalibration_entry(fit90), "raw", "bands", "crosscalibration"))))
})

test_that("pairing, metadata, repeated rows and continuous target coordinates are retained", {
  f <- radial_crosscalibration_fixture()
  selected <- c(4, 1, 7, 3, 4, 2, 6, 5)
  pts <- f$pts_uncal[selected, ]
  pts$note <- NA_character_
  ref <- f$pts_cal[selected, "off_axis", drop = FALSE]
  fit <- crosscalibrate_geometry(pts, ref, "radial_projection", f$center)
  expect_identical(fit$ds[names(pts)], pts)
  expect_identical(fit$ds$off_axis, ref$off_axis)
  expect_equal(unname(coef(fit$model)), c(40, 5, 2))
  expect_equal(nobs(fit$model), length(selected))
  # A center outside the image frame is permitted and never changed.
  outside <- c(-10, 0)
  pts$x <- pts$x + outside[1] - f$center[1]
  pts$y_down <- pts$y_down + outside[2] - f$center[2]
  shifted <- crosscalibrate_geometry(pts, ref, "radial_projection", outside)
  expect_identical(shifted$center_of_vision, outside)
  expect_equal(coef(shifted$model), coef(fit$model))
})

test_that("radial input checks validate the data contract without dropping observations", {
  f <- radial_crosscalibration_fixture()
  args <- list(pts_uncal = f$pts_uncal, pts_cal = f$pts_cal,
               model = "radial_projection", center_of_vision = f$center)
  run <- function(...) {
    changes <- list(...)
    candidate <- args
    candidate[names(changes)] <- changes
    do.call(crosscalibrate_geometry, candidate)
  }
  expect_error(run(model = NULL), "model")
  expect_error(do.call(crosscalibrate_geometry, args[names(args) != "model"]),
               "must be supplied")
  expect_error(run(model = "direction_model"), "radial_projection")
  expect_error(run(pts_cal = f$reference_coord), "off_axis")
  expect_error(run(pts_uncal = f$pts_uncal[-1, ]), "same number of rows")
  expect_error(run(center_of_vision = NULL), "center_of_vision")
  expect_error(run(center_of_vision = c(70.5, 50)), "integer")
  for (bad in c(-1, NA_real_, Inf, 4)) {
    ref <- f$pts_cal
    ref$off_axis[2] <- bad
    expect_error(run(pts_cal = ref), "off_axis")
  }
  for (bad in c(0, 181, NA_real_))
    expect_error(run(normalization_angle = bad), "normalization_angle")
  for (bad in c(0, 1.5)) expect_error(run(degree = bad), "degree")
  ref <- f$pts_cal
  ref$off_axis <- .5
  expect_error(run(pts_cal = ref), "polynomial degree")
  expect_error(run(pts_cal = ref[FALSE, ], pts_uncal = f$pts_uncal[FALSE, ]),
               "polynomial degree")
  pts <- f$pts_uncal
  pts$x[2] <- NA_real_
  expect_error(run(pts_uncal = pts), "NA")
  pts <- f$pts_uncal
  pts$radius <- 0
  expect_error(run(pts_uncal = pts), "reserved")
  expect_error(run(pts_uncal = data.frame(col = 1:7, row = 1:7)), "x, y_down")
  expect_error(run(zenith_coord_uncal = f$center), "unused argument")
  # Zero fitted scale cannot define a normalized projection.
  pts <- f$pts_uncal
  pts$x <- f$center[1]
  pts$y_down <- f$center[2]
  expect_error(run(pts_uncal = pts), "finite positive")
})

anisotropic_crosscalibration_fixture <- function(reference = "anisotropic_projection", scale = 1) {
  parameters <- list(off_axis_coef = c(1.1, .12, -.025),
    sin_internal_azimuth_coef = c(0, 1), cos_internal_azimuth_coef = c(-1, 0),
    atan_scale = .6)
  ref_center <- c(33, 22)
  geometry <- list(model = reference, center_of_vision = ref_center,
    parameters = if (reference == "anisotropic_projection") parameters else 2 / pi,
    normalization_radius = 40, normalization_angle = 90, max_off_axis_angle = 150)
  ig <- set_internal_geometry(c(80, 60), geometry)
  ref_coord <- coord_from_indices(
    c(34, 33, 18, 65, 8, 75, 45, 70, 22, 58, 12, 40),
    c(4, 4, 15, 23, 46, 50, 40, 6, 30, 55, 10, 42))
  digitized <- ref_coord + .1
  pts_cal <- add_directions(indices_from_coord(digitized$x, digitized$y_down),
                            internal_geometry = ig)
  center <- c(36, 24) * scale
  pts_uncal <- data.frame(x = center[1] + scale * (ref_coord$x - ref_center[1]),
    y_down = center[2] + scale * (ref_coord$y_down - ref_center[2]),
    id = letters[seq_len(nrow(ref_coord))])
  list(pts_uncal = pts_uncal, pts_cal = pts_cal, center = center,
       dim = c(80, 60) * scale, parameters = parameters)
}

fit_anisotropic_fixture <- function(f) {
  crosscalibrate_geometry(f$pts_uncal, f$pts_cal, "anisotropic_projection",
    center_of_vision = f$center, width_uncal = f$dim[1], height_uncal = f$dim[2],
    atan_scale = f$parameters$atan_scale)
}

test_that("anisotropic cross-calibration follows the reference-to-registry workflow", {
  for (reference in c("radial_projection", "anisotropic_projection")) {
    f <- anisotropic_crosscalibration_fixture(reference)
    original <- f
    fit <- fit_anisotropic_fixture(f)
    expect_identical(f, original)
    expect_named(fit, c("ds", "model", "parameters", "center_of_vision"))
    expect_identical(fit$ds[names(f$pts_uncal)], f$pts_uncal)
    expect_identical(fit$ds$off_axis, f$pts_cal$off_axis)
    expect_identical(fit$ds$internal_azimuth, f$pts_cal$internal_azimuth)
    expect_identical(fit$center_of_vision, f$center)
    expect_named(fit$model, c("off_axis", "sin_internal_azimuth", "cos_internal_azimuth"))
    expect_true(all(vapply(fit$model, inherits, logical(1), "lm")))
    expect_true(all(vapply(fit$model, function(x) attr(terms(x), "intercept") == 0, logical(1))))
    expect_named(fit$parameters, names(f$parameters))
    if (reference == "anisotropic_projection") {
      expect_equal(fit$parameters, f$parameters)
      expect_equal(fit$ds$fitted_off_axis, f$pts_cal$off_axis)
      expect_equal(fit$ds$fitted_internal_azimuth, f$pts_cal$internal_azimuth)
      expect_equal(fit$ds$angular_error, rep(0, nrow(f$pts_cal)), tolerance = 5e-8)
    }

    entry <- new_registry_entry("anisotropic_test", device = "Test device")
    entry <- add_signature(entry, "raw", dim = f$dim,
      validated_with_libraw = NA_character_, validated_with_exiftool = NA_character_,
      rules = list(model = list(group = "IFD0", tag = "Model", label = "Model",
                                expected = "Test device")))
    entry <- add_band_spec(entry, "raw", "bands")
    entry <- add_geometry_spec(entry, "raw", "bands", "crosscalibration",
      model = "anisotropic_projection", parameters = fit$parameters,
      center_of_vision = fit$center_of_vision)
    spec <- get_geometry_spec(entry, "raw", "bands", "crosscalibration")
    expect_null(spec$max_observed_off_axis_angle)
    ig <- set_internal_geometry(entry$raw$dim, spec)
    indices <- indices_from_coord(f$pts_uncal$x, f$pts_uncal$y_down)
    predicted <- add_directions(indices, internal_geometry = ig)
    expect_equal(predicted$off_axis, fit$ds$fitted_off_axis)
    difference <- predicted$internal_azimuth - fit$ds$fitted_internal_azimuth
    expect_equal(atan2(sin(difference), cos(difference)), rep(0, nrow(predicted)))
    # The top-of-sensor direction straddles the circular boundary.
    expect_lt(fit$ds$internal_azimuth[1], .1)
    expect_gt(fit$ds$internal_azimuth[2], 2 * pi - .1)
  }
})

test_that("image dimensions and declared center govern anisotropic preprocessing", {
  f <- anisotropic_crosscalibration_fixture(scale = 2)
  fit <- fit_anisotropic_fixture(f)
  expect_equal(fit$parameters, f$parameters)
  expect_true(any(abs(f$pts_uncal$x - round(f$pts_uncal$x)) < 1e-10))
  expected_radius <- sqrt((f$pts_uncal$x - f$center[1])^2 +
                           (f$pts_uncal$y_down - f$center[2])^2) /
    sqrt((f$dim[1] / 2)^2 + (f$dim[2] / 2)^2)
  expect_equal(fit$ds$relative_radius, expected_radius)
  expect_equal(fit$ds$atan_radius, atan(expected_radius / .6))
  selected <- c(4, 1, 2, 4, 12, 6, 9, 3, 5, 8)
  f$pts_uncal <- f$pts_uncal[selected, ]
  f$pts_uncal$note <- NA_character_
  f$pts_cal <- f$pts_cal[selected, c("off_axis", "internal_azimuth")]
  duplicated <- fit_anisotropic_fixture(f)
  expect_identical(duplicated$ds[names(f$pts_uncal)], f$pts_uncal)
  expect_equal(duplicated$parameters, fit$parameters)
  expect_equal(nobs(duplicated$model$off_axis), length(selected))
})

test_that("anisotropic residuals are reported without compensating or filtering", {
  f <- anisotropic_crosscalibration_fixture()
  f$pts_cal$off_axis[c(1, 6)] <- f$pts_cal$off_axis[c(1, 6)] + c(.03, -.02)
  f$pts_cal$internal_azimuth[c(2, 9)] <-
    (f$pts_cal$internal_azimuth[c(2, 9)] + c(.04, -.03)) %% (2 * pi)
  fit <- fit_anisotropic_fixture(f)
  expect_identical(fit$ds$off_axis, f$pts_cal$off_axis)
  expect_identical(fit$ds$internal_azimuth, f$pts_cal$internal_azimuth)
  expect_equal(fit$ds$fitted_off_axis, unname(predict(fit$model$off_axis, fit$ds)))
  expect_equal(fit$ds$off_axis_residual, unname(residuals(fit$model$off_axis)))
  difference <- fit$ds$internal_azimuth - fit$ds$fitted_internal_azimuth
  expect_equal(fit$ds$internal_azimuth_residual, atan2(sin(difference), cos(difference)))
  expect_equal(fit$ds$angular_error, calc_spherical_distance(
    fit$ds$off_axis, fit$ds$internal_azimuth,
    fit$ds$fitted_off_axis, fit$ds$fitted_internal_azimuth))
  expect_true(any(fit$ds$angular_error > 0))
  expect_true(all(abs(fit$ds$internal_azimuth_residual) <= pi))
  ig <- set_internal_geometry(f$dim, list(model = "anisotropic_projection",
    center_of_vision = f$center, parameters = fit$parameters))
  predicted <- add_directions(indices_from_coord(f$pts_uncal$x, f$pts_uncal$y_down), ig)
  expect_equal(predicted$off_axis, fit$ds$fitted_off_axis)
  expect_equal(predicted$internal_azimuth, fit$ds$fitted_internal_azimuth)
})

test_that("anisotropic fitting checks its new contract and estimability", {
  f <- anisotropic_crosscalibration_fixture()
  args <- list(pts_uncal = f$pts_uncal, pts_cal = f$pts_cal,
    model = "anisotropic_projection", center_of_vision = f$center,
    width_uncal = f$dim[1], height_uncal = f$dim[2], atan_scale = .6)
  run <- function(...) {
    changes <- list(...)
    candidate <- args
    candidate[names(changes)] <- changes
    do.call(crosscalibrate_geometry, candidate)
  }
  expect_error(run(center_of_vision = NULL), "center_of_vision")
  expect_error(run(center_of_vision = c(36.5, 24)), "integer")
  expect_error(run(width_uncal = NULL), "width")
  expect_error(run(height_uncal = 0), "height")
  expect_error(run(atan_scale = 0), "atan_scale")
  expect_error(run(pts_cal = f$pts_cal["off_axis"]), "internal_azimuth")
  for (column in c("off_axis", "internal_azimuth")) {
    for (bad in c(-1, NA_real_, Inf, 7)) {
      points <- f$pts_cal
      points[[column]][1] <- bad
      expect_error(run(pts_cal = points), column)
    }
  }
  bad <- f$pts_uncal
  bad$fitted_internal_azimuth <- 0
  expect_error(run(pts_uncal = bad), "reserved")
  expect_error(run(degree = nrow(f$pts_cal) + 1), "polynomial degree")
  bad <- f$pts_uncal
  bad$x <- f$center[1] + seq_len(nrow(bad))
  bad$y_down <- f$center[2]
  expect_error(run(pts_uncal = bad), "sin_internal_azimuth model")
})

test_that("a fitted circle center is explicitly snapped before cross-calibration", {
  f <- radial_crosscalibration_fixture()
  angles <- seq(-.5, 2, length.out = 10)
  center_points <- data.frame(x = 170.2 + 100 * cos(angles),
                              y_down = 139.8 + 100 * sin(angles))
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  write_points(center_points, file)
  estimated_center <- calc_center_of_vision(read_points(file))
  center <- round(estimated_center)
  expect_equal(estimated_center, c(x = 170.2, y_down = 139.8))
  expect_equal(unname(center), f$center)
  fit <- crosscalibrate_geometry(f$pts_uncal, f$pts_cal,
    model = "radial_projection", center_of_vision = center)
  expect_identical(fit$center_of_vision, center)
  expect_equal(unname(coef(fit$model)), c(40, 5, 2))
  entry <- radial_crosscalibration_entry(fit)
  spec <- get_geometry_spec(entry, "raw", "bands", "crosscalibration")
  expect_identical(spec$center_of_vision, center)
  ig <- set_internal_geometry(entry$raw$dim, spec)
  expect_identical(names(ig), c("off_axis", "internal_azimuth"))
})
