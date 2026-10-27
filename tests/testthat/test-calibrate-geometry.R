simple_method_points <- function(center = c(10.25, 6.75), direction = 0,
                                  n = 19, coefficients = c(120, 3, -2)) {
  angle <- (seq_len(n) - 1) * 5 * pi / 180
  radius <- coefficients[1] * angle + coefficients[2] * angle^2 +
    coefficients[3] * angle^3
  data.frame(x = center[1] + radius * cos(direction),
             y_down = center[2] + radius * sin(direction))
}

test_that("simple method uses every pin and preserves the radial fit", {
  centers <- list(c(10.25, 6.75), c(11.5, 8.25), c(12.75, 7.5))
  series <- Map(function(center, direction) simple_method_points(center, direction),
                 centers, c(0, pi / 2, pi))
  original <- series
  cal <- calibrate_geometry(series)
  expect_identical(series, original)
  expect_equal(nrow(cal$ds), 57)
  expect_equal(cal$ds$off_axis, rep(seq(0, 90, 5) * pi / 180, 3))
  expect_named(cal$ds, c("off_axis", "radius"))
  expect_s3_class(cal$model, "lm")
  expect_equal(unname(coef(cal$model)), c(120, 3, -2), tolerance = 1e-10)
  expect_equal(cal$center_of_vision, c(x = 11.5, y_down = 7.5))
  expect_equal(cal$normalization_radius, 120*pi/2 + 3*(pi/2)^2 - 2*(pi/2)^3)
  expect_equal(predict(cal$model, data.frame(off_axis = c(0, 57.5, 90) * pi / 180)),
    setNames(calc_relative_radius(c(0, 57.5, 90), c(120, 3, -2)), 1:3))
  expect_named(cal, c("ds", "model", "normalization_radius",
                      "normalization_angle", "projection_coef", "center_of_vision"))
})

test_that("the last pin remains in shorter and single series", {
  p <- simple_method_points(n = 10)
  cal <- calibrate_geometry(list(p))
  expect_equal(nrow(cal$ds), 10)
  expect_equal(tail(cal$ds$off_axis, 1), pi / 4)
  expect_equal(tail(cal$ds$radius, 1), sqrt(sum((p[10, ] - p[1, ])^2)))
  expect_equal(cal$center_of_vision, c(x = 10.25, y_down = 6.75))
  expect_equal(unname(coef(cal$model)), c(120, 3, -2), tolerance = 1e-9)
  short <- simple_method_points(n = 2, coefficients = c(120, 0, 0))
  expect_equal(calibrate_geometry(list(short), degree = 1)$projection_coef, 2 / pi)
})

test_that("normalization changes scale without changing the pixel projection", {
  p <- list(simple_method_points())
  base <- calibrate_geometry(p)
  for (reference in c(60, 120)) {
    cal <- calibrate_geometry(p, normalization_angle = reference)
    expect_identical(cal$normalization_angle, reference)
    expect_equal(coef(cal$model), coef(base$model))
    expect_equal(calc_relative_radius(reference, cal$projection_coef), 1)
    angle <- c(0, 30, 57.5, 90, 120)
    expect_equal(cal$normalization_radius * calc_relative_radius(angle, cal$projection_coef),
                 base$normalization_radius * calc_relative_radius(angle, base$projection_coef))
    expect_equal(calc_off_axis_angle(
      calc_relative_radius(angle, cal$projection_coef), cal$projection_coef,
      max_off_axis_angle = 120), angle, tolerance = 1e-9)
  }
})

test_that("distributed pin-only examples preserve the earlier principal fit", {
  paths <- system.file("external", c("a_DSCN7172.csv", "b_DSCN7162.csv"),
                        package = "rcaiman.registry")
  pts <- lapply(paths, read_points)
  expect_equal(vapply(pts, nrow, integer(1)), c(19L, 19L))
  cal <- calibrate_geometry(pts)
  expect_equal(cal$normalization_radius, 377.390549072412, tolerance = 1e-12)
  expect_equal(cal$projection_coef,
    c(.643837804828532, .0293783961446958, -.0216282262362101), tolerance = 1e-12)
  expect_equal(cal$center_of_vision, c(x = 638.463, y_down = 495.7835))
})

test_that("calibration results supply the registry and core contracts", {
  cal <- calibrate_geometry(list(simple_method_points()), normalization_angle = 60)
  entry <- new_registry_entry("simple", device = "Test device")
  entry <- add_signature(entry, "raw", dim = c(20, 12),
    validated_with_libraw = NA_character_, validated_with_exiftool = NA_character_,
    rules = list(model = list(group = "IFD0", tag = "Model",
                              label = "Model", expected = "Test device")))
  entry <- add_band_spec(entry, "raw", "bands")
  entry <- add_geometry_spec(entry, "raw", "bands", "simple_method",
    model = "radial_projection", parameters = cal$projection_coef,
    center_of_vision = round(cal$center_of_vision),
    normalization_radius = cal$normalization_radius,
    normalization_angle = cal$normalization_angle)
  geometry <- get_geometry_spec(entry, "raw", "bands", "simple_method")
  expect_null(geometry$max_observed_off_axis_angle)
  ig <- set_internal_geometry(entry$raw$dim, geometry)
  expect_true(all(is.finite(values(ig))))
  expect_equal(dim(ig), c(12, 20, 2))
})

test_that("simple method rejects unsupported pin counts and singular fits", {
  for (n in c(1, 20)) {
    expect_error(calibrate_geometry(list(simple_method_points(n = n))), "2 to 19")
  }
  expect_error(calibrate_geometry(list()), "non-empty")
  expect_error(calibrate_geometry(list(simple_method_points(n = 3))), "polynomial degree")
  expect_error(calibrate_geometry(list(simple_method_points()), degree = 2.5), "integer")
  for (reference in list(0, -1, 181, NA_real_, Inf, c(60, 90))) {
    expect_error(calibrate_geometry(list(simple_method_points()),
                                    normalization_angle = reference))
  }
  expect_error(calibrate_geometry(list(simple_method_points(coefficients = c(0, 0, 0)))),
               "finite positive")
  bad <- simple_method_points()
  bad$x[2] <- NA_real_
  expect_error(calibrate_geometry(list(bad)), "NA")
})


test_that("direct and cross calibration models share the radian prediction contract", {
  center <- c(10, 7)
  points <- simple_method_points(center = center)
  directions <- data.frame(off_axis = seq(0, 90, 5) * pi / 180)
  direct <- calibrate_geometry(list(points), normalization_angle = 60)
  cross <- crosscalibrate_geometry(points, directions, "radial_projection",
    center_of_vision = center, normalization_angle = 60)
  expect_equal(direct$ds$off_axis, directions$off_axis)
  expect_equal(unname(coef(direct$model)), unname(coef(cross$model)))
  expect_equal(direct$projection_coef, cross$projection_coef)
  expect_equal(direct$normalization_radius, cross$normalization_radius)
  expect_equal(direct$normalization_angle, 60)
  query <- data.frame(off_axis = c(0, pi / 6, 57.5 * pi / 180, pi / 2))
  expect_equal(predict(direct$model, query), predict(cross$model, query))
  expect_equal(unname(predict(direct$model, query)),
    direct$normalization_radius * calc_relative_radius(
      query$off_axis, direct$projection_coef, units = "radian"))
})
