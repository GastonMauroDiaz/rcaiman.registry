#' Cross-calibrate geometry
#'
#' @description
#' Estimate a lens projection for an uncalibrated camera by referencing a
#' calibrated camera photographed from the same location.
#'
#' @details
#' The calibrated and uncalibrated photographs must be taken with matching
#' entrance pupils and aligned optical axes. Corresponding points digitized in
#' both images are used to transfer zenith angles from the calibrated image to
#' the uncalibrated image and fit a polynomial lens projection.
#'
#' This method is intended for cases where a camera calibrated with a method of
#' higher accuracy than [calibrate_geometry()] is available, or when several
#' cameras should be cross-calibrated against a common reference camera.
#'
#' Points must be digitized in tandem. The split-view feature in
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects)
#' facilitates this task. See [calibrate_geometry()] for conceptual background.
#'
#' @param pts_uncal,pts_cal `data.frame` with at least columns `"row"` and
#'   `"col"`, giving raster coordinates of corresponding digitized points in the
#'   uncalibrated and calibrated images, respectively.
#' @param zenith_col_row_uncal,zenith_col_row_cal numeric vectors of length two.
#'   Raster coordinates of the zenith in the uncalibrated and calibrated images,
#'   respectively. See [calc_zenith_col_row()].
#' @param diameter_cal numeric vector of length one. Image diameter (pixels) of
#'   the calibrated camera.
#' @param projection_coef numeric vector. Lens projection coefficients of the
#'   calibrated camera.
#' @param degree numeric vector of length one. Polynomial degree used to fit the
#'   lens projection of the uncalibrated camera (default 3).
#'
#' @return List with the following components:
#' \describe{
#'   \item{`ds`}{`data.frame` with zenith angle (`theta`, radians) and radial
#'     distance (`px`, pixels) for the uncalibrated camera.}
#'   \item{`model`}{`lm` object fitted to radial distance as a polynomial
#'     function of zenith angle.}
#'   \item{`horizon_radius`}{numeric vector of length one. Predicted radial
#'     distance (pixels) at 90 deg.}
#'   \item{`projection_coef`}{numeric vector. Polynomial coefficients normalized
#'     by `horizon_radius`.}
#' }
#'
#' @seealso [calibrate_geometry()], [calc_zenith_col_row()]
#'
#' @export
crosscalibrate_geometry <- function(pts_uncal,
                                pts_cal,
                                zenith_col_row_uncal,
                                zenith_col_row_cal,
                                diameter_cal,
                                projection_coef,
                                degree = 3) {

  .check_vector(zenith_col_row_uncal, "numeric", 2, sign = "positive")
  .check_vector(zenith_col_row_cal, "numeric", 2, sign = "positive")
  .check_vector(diameter_cal, "numeric", 1, sign = "positive")
  .check_vector(projection_coef, "numeric", sign = "any")
  .check_vector(degree, "integerish", 1, sign = "positive")

  csv_uncal <- utils::read.csv(path_to_csv_uncal)
  csv_uncal <- cbind(csv_uncal$X, csv_uncal$Y)
  csv_cal <- utils::read.csv(path_to_csv_cal)
  csv_cal <- cbind(csv_cal$X, csv_cal$Y)

  ## center in (0,0)
  csv_uncal[, 1] <- csv_uncal[, 1] - zenith_col_row_uncal[1]
  csv_uncal[, 2] <- csv_uncal[, 2] - zenith_col_row_uncal[2]
  csv_cal[, 1] <- csv_cal[, 1] - zenith_col_row_cal[1]
  csv_cal[, 2] <- csv_cal[, 2] - zenith_col_row_cal[2]

  csv_uncal <- geometry::cart2pol(csv_uncal)
  csv_cal <- geometry::cart2pol(csv_cal)
  px <- csv_uncal[, 2]

  angle <- seq(0, 90,  length.out = 90)
  R <- calc_relative_radius(angle, projection_coef)
  inv_fun <- splinefun(R * diameter_cal/2, angle * pi / 180)

  theta <- inv_fun(csv_cal[, 2])

  fit <- lm(px ~ poly(theta, degree, raw = TRUE) - 1)
  horizon_radius <- stats::predict(fit, data.frame(theta = pi / 2)) %>% unname()

  list(ds = data.frame(theta, px),
       model = fit,
       horizon_radius = horizon_radius,
       projection_coef = (coefficients(fit) / horizon_radius) %>% unname())
}
