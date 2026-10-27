test_that("cross calibration estimates each axis independently and preserves pairs", {
  u <- data.frame(off_axis = c(.6, .02, .1, .04, .9, .3),
                  dn = c(140, 205, 198, 200, 100, 180), group = "target",
                  note = NA_character_)
  c <- data.frame(off_axis = c(.61, .05, .02, .04, .91, .31),
                  dn = c(70, 102, 99, 100, 50, 90), group = "reference")
  fit <- crosscalibrate_radiometry(u, c, degree = 2)
  weights <- function(d) {
    i <- order(d)[1:3]
    w <- numeric(length(d))
    w[i] <- (1 / d[i]^2) / sum(1 / d[i]^2)
    w
  }
  ru <- sum(weights(u$off_axis) * u$dn)
  rc <- sum(weights(c$off_axis) * c$dn)
  expected <- (u$dn / ru) / (c$dn / rc)
  expect_equal(fit$reference_dn, c(uncal = ru, cal = rc))
  expect_equal(fit$profile_uncal$reference_weight, weights(u$off_axis))
  expect_equal(fit$profile_cal$reference_weight, weights(c$off_axis))
  expect_identical(fit$profile_uncal[names(u)], u)
  expect_identical(fit$profile_cal[names(c)], c)
  expect_equal(fit$ds$relative_response, expected)
  expect_equal(fit$ds$off_axis, u$off_axis)
  expect_equal(fit$ds$off_axis_cal, c$off_axis)
  expect_equal(fit$vignetting_coef,
    unname(coef(lm(I(expected - 1) ~ poly(u$off_axis, 2, raw = TRUE) - 1))))
  u$dn <- u$dn * 7
  c$dn <- c$dn * .3
  changed <- crosscalibrate_radiometry(u, c, degree = 2)
  expect_equal(changed$vignetting_coef, fit$vignetting_coef)
  expect_equal(changed$ds$relative_response, fit$ds$relative_response)
})

test_that("a known response is recovered when the axis reference is observed", {
  angle <- c(.4, 0, .1, .8, .2, 1.2)
  response <- 1 - .12 * angle - .08 * angle^2
  illumination <- 150 + 20 * sin(angle)
  u <- data.frame(off_axis = angle, dn = 2 * illumination * response)
  c <- data.frame(off_axis = angle, dn = illumination)
  fit <- crosscalibrate_radiometry(u, c, degree = 2)
  expect_equal(fit$vignetting_coef, c(-.12, -.08))
  expect_equal(fit$ds$relative_response, response)
  expect_equal(fit$reference_dn, c(uncal = 300, cal = 150))
  expect_equal(fit$profile_uncal$reference_weight, c(0, 1, 0, 0, 0, 0))
  expect_equal(unname(1 - predict(fit$model, fit$ds)), response)
})

test_that("cross calibration requires a single valid group per image", {
  x <- data.frame(off_axis = c(.01, .1, .3, .6), dn = c(100, 98, 91, 80))
  y <- x
  y$group <- "reference"
  expect_s3_class(crosscalibrate_radiometry(x, y, degree = 2)$model, "lm")
  for (group in list(c("a", "b", "a", "a"), rep(NA_character_, 4),
                     c("a", NA, "a", "a"), rep("", 4), rep(" ", 4))) {
    x$group <- group
    expect_error(crosscalibrate_radiometry(x, y, degree = 2), "one.*group")
    expect_error(crosscalibrate_radiometry(y, x, degree = 2), "one.*group")
  }
  x$group <- factor(rep("target", 4))
  expect_s3_class(crosscalibrate_radiometry(x, y, degree = 2)$model, "lm")
})

test_that("invalid paired measurements are not silently discarded", {
  x <- data.frame(off_axis = c(.01, .1, .3, .6), dn = c(100, 98, 91, 80))
  for (value in c(NA_real_, Inf, 0, -1)) {
    bad <- x
    bad$dn[1] <- value
    expect_error(crosscalibrate_radiometry(bad, x, degree = 2))
  }
  expect_error(crosscalibrate_radiometry(x, x[1:3, ], degree = 2), "same number")
  expect_error(crosscalibrate_radiometry(x, x, k = 5), "at least")
  expect_error(crosscalibrate_radiometry(x, x, k = 0))
  expect_error(crosscalibrate_radiometry(x, x, degree = 7), "degree")
  expect_error(crosscalibrate_radiometry(x, x, degree = 0))
  bad <- x
  bad$off_axis <- 90
  expect_error(crosscalibrate_radiometry(bad, x), "radians")
  bad$off_axis <- .1
  expect_error(crosscalibrate_radiometry(bad, x, degree = 2), "degree")
  bad <- x
  bad$reference_dn <- 100
  expect_error(crosscalibrate_radiometry(bad, x), "reference_dn")
})

test_that("sampling two original grids yields a reusable radiometry specification", {
  geometry <- list(model = "radial_projection", parameters = equidistant_coef,
                    center_of_vision = c(12, 10), normalization_radius = 16,
                    normalization_angle = 90)
  ig_uncal <- set_internal_geometry(c(24, 20), geometry)
  geometry_cal <- geometry
  geometry_cal$center_of_vision <- c(15, 12)
  ig_cal <- set_internal_geometry(c(30, 24), geometry_cal)
  ff_cal <- flat_field_image(ig_cal$off_axis, "simple_polynomial", c(-.1, -.02))
  r_cal <- apply_flat_field((200 + ig_cal$off_axis) * ff_cal, ff_cal)
  response <- flat_field_image(ig_uncal$off_axis, "simple_polynomial", c(-.15, -.03))
  r_uncal <- 2 * (200 + ig_uncal$off_axis) * response
  query_uncal <- data.frame(col = c(12, 14, 16, 18, 20, 22), row = 10)
  query_directions <- add_directions(query_uncal, ig_uncal)
  candidates_uncal <- expand.grid(col = 8:23, row = 8:12)
  candidates_cal <- transform(candidates_uncal, col = col + 3, row = row + 2)
  su <- sample_radiometry(r_uncal, ig_uncal, query_directions, candidates_uncal,
                          search_radius = 2, group = "target")
  sc <- sample_radiometry(r_cal, ig_cal, query_directions, candidates_cal,
                          search_radius = 2, group = "reference")
  expect_true(all(su$measurements$n_samples == 1))
  expect_equal(su$measurements$off_axis, sc$measurements$off_axis)
  fit <- crosscalibrate_radiometry(su$measurements, sc$measurements, degree = 2)
  u <- su$measurements
  c <- sc$measurements
  ru <- weighted.mean(u$dn[1:3], 1 / u$off_axis[1:3]^2)
  rc <- weighted.mean(c$dn[1:3], 1 / c$off_axis[1:3]^2)
  expect_equal(fit$ds$relative_response, (u$dn / ru) / (c$dn / rc))
  # The local estimate need not equal the true axis DN; do not assert exact
  # recovery of the generating curve when all observed angles are nonzero.
  entry <- new_registry_entry("cross_radiometry_test", device = "Test device")
  entry <- add_signature(entry, "raw", dim = c(24, 20),
    validated_with_libraw = NA_character_, validated_with_exiftool = NA_character_,
    rules = list(model = list(group = "IFD0", tag = "Model",
                              label = "Model", expected = "Test device")))
  entry <- add_band_spec(entry, "raw", "bands")
  entry <- add_geometry_spec(entry, "raw", "bands", "radial",
    model = geometry$model, parameters = geometry$parameters,
    center_of_vision = geometry$center_of_vision,
    normalization_radius = geometry$normalization_radius,
    normalization_angle = geometry$normalization_angle)
  entry <- add_radiometry_spec(entry, "raw", "bands", "radial", "cross",
    model = "simple_polynomial", parameters = list("5" = list(Blue = fit$vignetting_coef)))
  spec <- get_radiometry_spec(entry, "raw", "bands", "radial", "cross")
  ff <- flat_field_image(ig_uncal$off_axis, spec$model, spec$parameters[["5"]]$Blue)
  expected <- calc_relative_response(u$off_axis, spec$model,
                                     spec$parameters[["5"]]$Blue, units = "radian")
  names(ff) <- "response"
  expect_equal(extract_dn(ff, query_uncal, use_window = FALSE)$response, expected)
  corrected <- apply_flat_field(r_uncal, ff)
  expect_equal(as.numeric(terra::values(corrected)),
               as.numeric(terra::values(r_uncal / ff)))
})
