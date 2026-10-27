angular_sampling_fixture <- function(width = 20, height = 10, radius = 10) {
  ig <- set_internal_geometry(c(width, height), list(
    model = "radial_projection", parameters = equidistant_coef,
    center_of_vision = c(width / 2, height / 2),
    normalization_radius = radius, normalization_angle = 90))
  r <- 100 + ig$off_axis
  indices <- expand.grid(col = seq_len(width), row = seq_len(height))
  indices <- rem_unselected(indices, !is.na(ig$off_axis) & !is.na(ig$internal_azimuth))
  list(ig = ig, r = r, indices = indices)
}

test_that("angular queries are not snapped to candidate cells", {
  f <- angular_sampling_fixture()
  indices <- data.frame(col = c(11, 12), row = c(5, 4))
  rays <- add_directions(indices, f$ig)
  query <- data.frame(off_axis = mean(rays$off_axis), internal_azimuth = pi / 4)
  empty <- sample_radiometry(f$r, f$ig, query, indices, search_radius = 1)
  expect_equal(empty$measurements$n_samples, 0L)
  expect_true(is.na(empty$measurements$dn))
  expect_equal(empty$measurements[names(query)], query)
  both <- sample_radiometry(f$r, f$ig, query, indices, search_radius = 7)
  expect_equal(both$measurements$n_samples, 2L)
  expect_equal(both$measurements$dn, mean(100 + rays$off_axis * 180 / pi))
  expect_equal(both$points$distance, rep(diff(rays$off_axis) / 2, 2))
})

test_that("shared queries sample each image grid and mask independently", {
  queries <- data.frame(off_axis = c(.07, .3, .6, .3),
                         internal_azimuth = c(.4, 1.2, 2.5, 1.2),
                         id = c("a", "b", "c", "b"), note = NA_character_)
  results <- list()
  for (i in 1:2) {
    f <- angular_sampling_fixture(20 * i, 10 * i, 10 * i)
    # A scientist's mask excludes different image cells in each system.
    candidates <- f$indices[f$indices$col %% 3 != 0, ]
    rays <- add_directions(candidates, f$ig)
    sampled <- sample_radiometry(f$r, f$ig, queries, candidates, search_radius = 12)
    expect_identical(sampled$measurements[names(queries)], queries)
    for (j in seq_len(nrow(queries))) {
      distance <- calc_spherical_distance(rays$off_axis, rays$internal_azimuth,
        queries$off_axis[j], queries$internal_azimuth[j])
      keep <- which(distance <= 12 * pi / 180)
      expect_setequal(sampled$points$sample_id[sampled$points$query_id == j], keep)
      expect_equal(sampled$measurements$n_samples[j], length(keep))
      expect_equal(sampled$measurements$dn[j], mean(100 + rays$off_axis[keep] * 180 / pi))
    }
    results[[i]] <- sampled
  }
  expect_identical(results[[1]]$measurements$off_axis, results[[2]]$measurements$off_axis)
  expect_false(identical(results[[1]]$measurements$n_samples, results[[2]]$measurements$n_samples))
})

test_that("query coordinates and indices are retained only as metadata", {
  f <- angular_sampling_fixture()
  query <- add_directions(data.frame(col = c(11, 12), row = c(5, 4)), f$ig)
  before <- sample_radiometry(f$r, f$ig, query, f$indices)
  query$col <- c(999, NA_real_)
  query$row <- c(-4.5, 0)
  query$x <- c(NA_real_, -1)
  query$y_down <- 999
  after <- sample_radiometry(f$r, f$ig, query, f$indices)
  expect_identical(after$points, before$points)
  expect_identical(after$measurements[names(query)], query)
  expect_identical(after$measurements$dn, before$measurements$dn)
})

test_that("angular queries require finite internal radians and unreserved columns", {
  f <- angular_sampling_fixture()
  q <- data.frame(off_axis = .1, internal_azimuth = 0)
  for (col in names(q)) {
    for (value in c(NA_real_, Inf, -1, 90)) {
      bad <- q
      bad[[col]] <- value
      expect_error(sample_radiometry(f$r, f$ig, bad, f$indices))
    }
  }
  bad <- q
  bad$internal_azimuth <- 2 * pi
  expect_error(sample_radiometry(f$r, f$ig, bad, f$indices), "radians")
  expect_error(sample_radiometry(f$r, f$ig, data.frame(col = 1, row = 1), f$indices),
                "off_axis, internal_azimuth")
  for (col in c("group", "query_id", "n_samples", "dn")) {
    bad <- q
    bad[[col]] <- 1
    expect_error(sample_radiometry(f$r, f$ig, bad, f$indices), "reserved")
  }
  q <- data.frame(off_axis = c(0, pi), internal_azimuth = 0)
  expect_equal(nrow(sample_radiometry(f$r, f$ig, q, f$indices)$measurements), 2)
})


test_that("Fibonacci directions are shared directly between radiometric samples", {
  queries <- fibonacci_directions(15, max_off_axis_angle = 60)
  f <- angular_sampling_fixture()
  g <- angular_sampling_fixture(40, 20, 20)
  a <- sample_radiometry(f$r, f$ig, queries, f$indices, search_radius = 8)
  b <- sample_radiometry(g$r, g$ig, queries, g$indices, search_radius = 8)
  expect_identical(a$measurements[names(queries)], queries)
  expect_identical(b$measurements[names(queries)], queries)
  expect_identical(a$measurements$query_id, b$measurements$query_id)
  expect_true(any(a$measurements$n_samples > 0 & b$measurements$n_samples > 0))
})
