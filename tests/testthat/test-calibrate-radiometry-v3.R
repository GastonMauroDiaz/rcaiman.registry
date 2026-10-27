radiometry_fixture <- function() {
  geometry <- list(model = "radial_projection", parameters = equidistant_coef,
                    center_of_vision = c(12, 10), normalization_radius = 16,
                    normalization_angle = 90)
  ig <- set_internal_geometry(c(24, 20), geometry)
  r <- round(1000 - 2 * (ig[["off_axis"]] * 16 / 90)^2)
  names(r) <- "Blue"
  query_coord <- data.frame(x = c(11.5, 13.5, 15.5, 17.5, 19.5), y_down = 9.5)
  query_indices <- indices_from_coord(query_coord$x, query_coord$y_down)
  query <- add_directions(query_indices, ig)
  # A manually chosen rectangle supplies a dense, stride-one candidate grid.
  candidates <- expand.grid(col = 8:21, row = 8:12)
  candidates <- rem_unselected(candidates, !is.na(ig$off_axis) & !is.na(ig$internal_azimuth))
  candidates <- rem_unselected(candidates, !is.na(r))
  list(r = r, ig = ig, query = query, candidates = candidates)
}

test_that("sampling exposes the pixels and means on the original band grid", {
  f <- radiometry_fixture()
  # Omit the manually excluded query-center pixel, without excluding the query.
  candidates <- f$candidates[!(f$candidates$col == 14 & f$candidates$row == 10), ]
  candidates$annotation <- NA_character_
  sampled <- sample_radiometry(f$r, f$ig, f$query, candidates,
                               search_radius = 12, group = "3")
  expected_candidates <- add_directions(candidates, f$ig)
  expected_queries <- f$query
  dn <- extract_dn(f$r, candidates, use_window = FALSE)$Blue
  for (i in seq_len(nrow(f$query))) {
    distances <- calc_spherical_distance(
      expected_candidates$off_axis, expected_candidates$internal_azimuth,
      expected_queries$off_axis[i], expected_queries$internal_azimuth[i])
    expected <- which(distances <= 12 * pi / 180)
    pixels <- sampled$points[sampled$points$query_id == i, ]
    expect_setequal(pixels$sample_id, expected)
    expect_equal(pixels$dn, dn[pixels$sample_id])
    expect_equal(pixels$distance, distances[pixels$sample_id], tolerance = 1e-7)
    expect_equal(pixels$col, candidates$col[pixels$sample_id])
    expect_equal(pixels$row, candidates$row[pixels$sample_id])
    expect_equal(sampled$measurements$dn[i], mean(dn[expected]))
    expect_equal(sampled$measurements$n_samples[i], length(expected))
  }
  expect_identical(sampled$measurements[names(f$query)], f$query)
  expect_true(anyDuplicated(sampled$points$sample_id) > 0)
  expect_true(all(is.na(sampled$points$annotation)))
  expect_true(all(sampled$points$group == "3"))
  expect_identical(names(f$r), "Blue")
  pixels <- sampled$points
  path <- tempfile(fileext = ".csv")
  write_points(cbind(coord_from_indices(pixels$col, pixels$row), pixels), path)
  read_back <- read_points(path)
  expect_equal(read_back$query_id, pixels$query_id)
  expect_equal(indices_from_coord(read_back$x, read_back$y_down),
                pixels[c("col", "row")])
})

test_that("the TIFF, maximum, sampling and fitting workflow recovers a response", {
  f <- radiometry_fixture()
  # Two source images illuminate complementary parts of the image.
  a <- f$r
  b <- f$r
  vals <- terra::values(f$r, mat = FALSE)
  left <- rep(seq_len(ncol(f$r)) <= 12, nrow(f$r))
  terra::values(a) <- vals - ifelse(left, 0, 100)
  terra::values(b) <- vals - ifelse(left, 100, 0)
  paths <- c(tempfile(fileext = ".tif"), tempfile(fileext = ".tif"))
  write_caim(a, paths[1], 16)
  write_caim(b, paths[2], 16)
  images <- lapply(paths, read_caim)
  r <- max(terra::rast(images))
  sampled_a <- sample_radiometry(r, f$ig, f$query, f$candidates,
                                 search_radius = 2, group = "3")
  sampled_b <- sample_radiometry(2 * r, f$ig, f$query, f$candidates,
                                 search_radius = 2, group = "9")
  measurements <- rbind(sampled_a$measurements, sampled_b$measurements)
  original <- measurements
  fit <- calibrate_radiometry(measurements, degree = 2)
  expected_coef <- -2 * (16 / (pi / 2))^2 / 1000
  expect_equal(sampled_a$measurements$n_samples, rep(1L, 5))
  expect_equal(fit$ds$reference_dn, rep(c(1000, 2000), each = 5))
  expect_equal(fit$vignetting_coef, c(0, expected_coef), tolerance = 1e-10)
  expect_identical(fit$ds$dn, measurements$dn)
  expect_identical(measurements, original)
  expect_equal(fit$ds$relative_response, rep(sampled_a$measurements$dn / 1000, 2))
  prediction <- 1 - predict(fit$model, data.frame(off_axis = measurements$off_axis))
  expect_equal(as.vector(prediction), fit$ds$relative_response)
  response <- calc_relative_response(fit$ds$off_axis, "simple_polynomial",
                                     fit$vignetting_coef, units = "radian")
  expect_equal(response, unname(1 - predict(fit$model, fit$ds)))
  ff <- flat_field_image(f$ig[["off_axis"]], "simple_polynomial", fit$vignetting_coef)
  expect_equal(as.vector(terra::values(ff)), vals / 1000, tolerance = 1e-10)
  entry <- new_registry_entry("radiometry_test", device = "Test device")
  entry <- add_signature(
    entry, id = "raw", dim = c(24, 20), validated_with_libraw = NA_character_,
    validated_with_exiftool = NA_character_,
    rules = list(model = list(group = "IFD0", tag = "Model",
                              label = "Model", expected = "Test device")))
  entry <- add_band_spec(entry, "raw", "bayer")
  entry <- add_geometry_spec(entry, "raw", "bayer", "radial",
                              model = "radial_projection",
                              parameters = equidistant_coef,
                              center_of_vision = c(12, 10),
                              normalization_radius = 16,
                              normalization_angle = 90)
  entry <- add_radiometry_spec(entry, "raw", "bayer", "radial", "simple",
                                model = "simple_polynomial",
                                parameters = list(`2.8` = list(Blue = fit$vignetting_coef)))
  expect_equal(get_radiometry_spec(entry, "raw", "bayer", "radial", "simple")$parameters,
                list(`2.8` = list(Blue = fit$vignetting_coef)))
})

test_that("sampling handles empty neighborhoods without hiding queries", {
  f <- radiometry_fixture()
  no_candidates <- f$candidates[FALSE, ]
  sample <- sample_radiometry(f$r, f$ig, f$query, no_candidates)
  expect_equal(nrow(sample$points), 0)
  expect_equal(nrow(sample$measurements), nrow(f$query))
  expect_true(all(sample$measurements$n_samples == 0))
  expect_true(all(is.na(sample$measurements$dn)))
  expect_error(calibrate_radiometry(sample$measurements), "NA")
  no_queries <- sample_radiometry(f$r, f$ig, f$query[FALSE, ], f$candidates)
  expect_equal(nrow(no_queries$points), 0)
  expect_equal(nrow(no_queries$measurements), 0)
  expect_error(calibrate_radiometry(no_queries$measurements), "at least one")
})

test_that("calibration retains the previous symmetric absolute-value normalization", {
  measurements <- data.frame(
    group = rep(c("12", "6"), each = 5),
    off_axis = rep(c(.08, .3, .6, .9, 1.2), 2),
    dn = c(-100, 96, 86, 75, 58, 150, 143, 130, 109, 82))
  result <- calibrate_radiometry(measurements)
  expected <- numeric(nrow(measurements))
  for (group in unique(measurements$group)) {
    i <- which(measurements$group == group)
    angle <- measurements$off_axis[i]
    dn <- abs(measurements$dn[i])
    reference <- splinefun(c(angle, -angle), rep(dn, 2))(0)
    expected[i] <- dn / reference
  }
  expect_equal(result$ds$relative_response, expected)
  expect_identical(result$ds$dn, measurements$dn)
  expect_true(result$ds$dn[1] < 0)
  old_fit <- lm((1 - expected) ~ poly(measurements$off_axis, 3, raw = TRUE) - 1)
  expect_equal(result$vignetting_coef, -unname(coef(old_fit)))
  shuffled <- calibrate_radiometry(measurements[c(10:6, 5:1), ])
  expect_equal(shuffled$vignetting_coef, result$vignetting_coef)
  expect_identical(shuffled$ds$dn, measurements$dn[c(10:6, 5:1)])
})

test_that("sampling and calibration reject incompatible inputs", {
  f <- radiometry_fixture()
  expect_error(sample_radiometry(f$r, terra::shift(f$ig, dx = 1), f$query,
                                 f$candidates), "same geometry")
  expect_error(sample_radiometry(c(f$r, f$r), f$ig, f$query, f$candidates),
               "single|one|layer")
  expect_error(sample_radiometry(f$r, f$ig, data.frame(x = 1, y_down = 2),
                                 f$candidates), "off_axis, internal_azimuth")
  bad <- f$candidates
  bad$col[1] <- 0.5
  expect_error(sample_radiometry(f$r, f$ig, f$query, bad), "integer-like")
  bad <- f$candidates
  bad$dn <- 1
  expect_error(sample_radiometry(f$r, f$ig, f$query, bad), "reserved")
  s <- sample_radiometry(f$r, f$ig, f$query, f$candidates)
  m <- s$measurements
  m$off_axis[1] <- 45
  expect_error(calibrate_radiometry(m), "radians")
  m <- s$measurements
  m$dn[] <- 0
  expect_error(calibrate_radiometry(m), "reference")
  expect_error(calibrate_radiometry(s$measurements, degree = 10), "degree")
  expect_error(calibrate_radiometry(s$measurements[1:2, ], degree = 3), "degree")
  no_group <- s$measurements
  no_group$group <- NULL
  expect_equal(calibrate_radiometry(no_group)$vignetting_coef,
                calibrate_radiometry(s$measurements)$vignetting_coef)
  missing <- f$r[[1]]
  missing[f$candidates$row[1], f$candidates$col[1]] <- NA
  expect_error(sample_radiometry(missing, f$ig, f$query, f$candidates), "NA")
  outside <- data.frame(off_axis = pi, internal_azimuth = 0)
  absent <- sample_radiometry(f$r, f$ig, outside, f$candidates)
  expect_equal(absent$measurements$n_samples, 0L)
  expect_true(is.na(absent$measurements$dn))
  m <- s$measurements
  m$group[1] <- NA_character_
  expect_error(calibrate_radiometry(m), "group")
})
