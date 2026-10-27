center_circle_points <- function(center, radius, angles) {
  data.frame(x = center[1] + radius * cos(angles),
             y_down = center[2] + radius * sin(angles))
}

test_that("circle centers remain continuous for complete circles and partial arcs", {
  center <- c(x = 645.3, y_down = 493.7)
  for (angles in list(seq(0, 2 * pi, length.out = 13)[-13],
                      c(.1, .5, .9), seq(-.4, 1.2, length.out = 9))) {
    points <- center_circle_points(center, 350, angles)
    points$id <- seq_len(nrow(points))
    points$note <- NA_character_
    before <- points
    estimated <- calc_center_of_vision(points)
    expect_equal(estimated, center, tolerance = 1e-10)
    expect_identical(names(estimated), c("x", "y_down"))
    expect_identical(points, before)
    expect_equal(round(estimated), c(x = 645, y_down = 494))
  }
})

test_that("the algebraic method retains its result on the supplied real point file", {
  path <- system.file("external/DSCN6089.csv", package = "rcaiman.registry")
  points <- read_points(path)
  A <- cbind(2 * points$x, 2 * points$y_down, 1)
  b <- points$x^2 + points$y_down^2
  original <- solve(crossprod(A), crossprod(A, b))
  expected <- c(x = unname(original[1]), y_down = unname(original[2]))
  estimate <- calc_center_of_vision(points)
  expect_equal(estimate, expected, tolerance = 1e-9)
  expect_equal(calc_center_of_vision(points[nrow(points):1, ]), estimate)
  # The same algebraic estimate is preserved on repeated observations.
  expect_equal(calc_center_of_vision(rbind(points, points)), estimate)
})

test_that("translation and scaling preserve circle-center estimates", {
  center <- c(x = 23.25, y_down = -5.4)
  points <- center_circle_points(center, 10, seq(0, 4, length.out = 15))
  points$x <- points$x + sin(seq_len(nrow(points))) / 10
  estimated <- calc_center_of_vision(points)
  transformed <- points
  transformed$x <- 3 * points$x + 1e8
  transformed$y_down <- 3 * points$y_down - 1e8
  actual <- calc_center_of_vision(transformed)
  expect_equal(unname(actual - c(1e8, -1e8)), unname(3 * estimated),
               tolerance = 1e-7)
})

test_that("an undetermined circle and invalid coordinates have explicit errors", {
  points <- center_circle_points(c(4, 5), 3, c(0, 1, 2, 3))
  expect_error(calc_center_of_vision(points[1:2, ]), "three non-collinear")
  expect_error(calc_center_of_vision(data.frame(x = 1:4, y_down = 2 * (1:4))),
               "three non-collinear")
  expect_error(calc_center_of_vision(data.frame(x = rep(2, 4), y_down = 1:4)),
               "three non-collinear")
  expect_error(calc_center_of_vision(points[c(1, 1, 2), ]), "three non-collinear")
  for (bad in c(NA_real_, Inf)) {
    invalid <- points
    invalid$x[1] <- bad
    expect_error(calc_center_of_vision(invalid), "NA|Inf")
  }
})
