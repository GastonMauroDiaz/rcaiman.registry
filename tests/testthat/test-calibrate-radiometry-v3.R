# These tests protect the alignment between original-coordinate annotations and
# processed image cells. They do not replace validation with real calibration
# cases, which remains a human-reviewed scientific procedure.

test_that("radiometry crop applies the same translation to image and points", {
  original <- set_rcaiman_geometry(terra::rast(nrows = 8, ncols = 10))
  terra::values(original) <- seq_len(terra::ncell(original))
  names(original) <- "Blue"
  z <- zenith_image(6, equidistant_coef)
  original_indices <- data.frame(
    col = c(3, 7, 4, 4, 4),
    row = c(2, 6, 3, 3, 3),
    id = c(
      "upper-left", "lower-right", "duplicate-a", "duplicate-b", "subpixel"
    )
  )
  original_coord <- coord_from_indices(
    original_indices$col,
    original_indices$row
  )
  original_coord$id <- original_indices$id
  original_coord$x[5] <- 3.9
  original_coord$y_down[5] <- 2.1
  rownames(original_coord) <- paste0("p", seq_len(nrow(original_coord)))

  processed <- .prepare_radiometry_image(
    original,
    z,
    snapped_zenith_coord = c(5, 4),
    horizon_radius = 3,
    is_horizon_circle_clipped = FALSE
  )
  processed_indices <- .prepare_radiometry_indices(
    original_coord,
    z,
    snapped_zenith_coord = c(5, 4),
    horizon_radius = 3,
    is_horizon_circle_clipped = FALSE
  )

  expect_equal(processed_indices$col, c(1, 5, 2, 2, 2))
  expect_equal(processed_indices$row, c(1, 5, 2, 2, 2))
  expect_identical(processed_indices$id, original_indices$id)
  expect_identical(rownames(processed_indices), rownames(original_coord))
  expect_equal(
    extract_dn(processed, processed_indices, use_window = FALSE)$Blue,
    extract_dn(original, original_indices, use_window = FALSE)$Blue
  )
})

test_that("radiometry expansion applies the same translation to image and points", {
  original <- set_rcaiman_geometry(terra::rast(nrows = 6, ncols = 6))
  terra::values(original) <- seq_len(terra::ncell(original))
  names(original) <- "Blue"
  z <- zenith_image(8, equidistant_coef)
  original_indices <- data.frame(
    col = c(1, 6, 2, 2, 1),
    row = c(1, 6, 3, 3, 1),
    id = c(
      "upper-left", "lower-right", "duplicate-a", "duplicate-b", "subpixel"
    )
  )
  original_coord <- coord_from_indices(
    original_indices$col,
    original_indices$row
  )
  original_coord$id <- original_indices$id
  original_coord$x[5] <- 0.9
  original_coord$y_down[5] <- 0.1

  processed <- .prepare_radiometry_image(
    original,
    z,
    snapped_zenith_coord = c(3, 3),
    horizon_radius = 4,
    is_horizon_circle_clipped = TRUE
  )
  processed_indices <- .prepare_radiometry_indices(
    original_coord,
    z,
    snapped_zenith_coord = c(3, 3),
    horizon_radius = 4,
    is_horizon_circle_clipped = TRUE
  )

  expect_equal(processed_indices$col, original_indices$col + 1)
  expect_equal(processed_indices$row, original_indices$row + 1)
  expect_identical(processed_indices$id, original_indices$id)
  expect_equal(
    extract_dn(processed, processed_indices, use_window = FALSE)$Blue,
    extract_dn(original, original_indices, use_window = FALSE)$Blue
  )
})

test_that("radiometry v3 exposes snapped coordinates and integer radius", {
  arguments <- names(formals(calibrate_radiometry))
  expect_true("snapped_zenith_coord" %in% arguments)
  expect_false("zenith_col_row" %in% arguments)

  query_path <- tempfile(fileext = ".csv")
  sampling_path <- tempfile(fileext = ".csv")
  writeLines("x,y_down\n0.5,0.5", query_path)
  writeLines("x,y_down\n0.5,0.5", sampling_path)
  calibration_data <- list(
    test = list(
      images = tempdir(),
      query_points = query_path,
      sampling_points = sampling_path
    )
  )
  expect_error(
    calibrate_radiometry(
      calibration_data,
      FALSE,
      snapped_zenith_coord = c(5.5, 5),
      horizon_radius = 3,
      projection_coef = equidistant_coef
    ),
    "integer-like"
  )
  expect_error(
    calibrate_radiometry(
      calibration_data,
      FALSE,
      snapped_zenith_coord = c(5, 5),
      horizon_radius = 3.5,
      projection_coef = equidistant_coef
    ),
    "integer-like"
  )
})
