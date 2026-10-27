#' Calibrate radiometry from sampled measurements
#'
#' @description
#' Normalize radiometric measurements to the optical axis and fit a radial
#' relative-response model.
#'
#' @details
#' [sample_radiometry()] prepares the measurements and the pixels contributing
#' to them. Review that sampling before calibration. Combine measurement
#' tables with `rbind()` to fit several acquisition groups together; their
#' `group` labels determine which measurements share a normalization reference.
#'
#' For each group, the measured profile is mirrored about zero off-axis angle.
#' The same absolute digital numbers are assigned to the positive and negative
#' angles. A cubic spline (`method = "fmm"`) through that symmetric profile
#' estimates the response at the optical axis. Repeated angles are averaged
#' for the spline only. A profile containing only zero angles uses their mean.
#' This preserves the normalization procedure of the simple method; `abs()`
#' is applied during calibration, not during sampling.
#'
#' Dividing the absolute measured digital numbers by that reference gives
#' `relative_response`. The reference must be finite and positive. This is an
#' optical-axis reference for vignetting, independent of the acquisition's
#' orientation and of the zenith radiance used in CIE sky models.
#'
#' The normalized groups are pooled to fit `1 - relative_response` as a
#' polynomial of off-axis angle without an intercept. Each measurement has
#' equal weight, irrespective of its number of sampled pixels. The resulting
#' response model equals one at zero angle, as in
#' [rcaiman::calc_relative_response()]. The returned `lm` uses `off_axis` in
#' radians, matching [rcaiman::add_directions()].
#'
#' @param measurements data frame, normally `sample_radiometry(...)$measurements`,
#'   with finite numeric columns `off_axis` in radians (`[0, pi]`) and `dn`.
#'   Optional `group` labels identify profiles normalized separately. Without
#'   that column, all rows belong to one group. Additional columns are retained
#'   in `ds`; `reference_dn` and `relative_response` must not already exist.
#'   Resolve or explicitly exclude missing measurements before fitting.
#' @param degree positive integer-like numeric vector of length one.
#'   Polynomial degree, from 1 to 6 as supported by
#'   [rcaiman::calc_relative_response()]. The observations must support all
#'   coefficients.
#'
#' @return list with the following components:
#'   \describe{
#'     \item{`ds`}{Measurement table in its original order, with `group`,
#'       `reference_dn`, and `relative_response`. Original `dn` values and
#'       `off_axis` angles in radians are preserved.}
#'     \item{`model`}{Fitted [stats::lm] object predicting
#'       `1 - relative_response` from `off_axis` in radians.}
#'     \item{`vignetting_coef`}{Numeric coefficients for
#'       `model = "simple_polynomial"` in [add_radiometry_spec()], in
#'       increasing order of degree and without an intercept.}
#'   }
#'
#' @seealso [sample_radiometry()] for acquisition guidance and a complete
#'   sampling example; [rcaiman::calc_relative_response()], [add_radiometry_spec()]
#' @export
#' @examples
#' \dontrun{
#' measurements <- rbind(
#'   samples_a$measurements,
#'   samples_b$measurements,
#'   samples_c$measurements,
#'   samples_d$measurements
#' )
#' calibration <- calibrate_radiometry(measurements)
#' plot(
#'   relative_response ~ off_axis,
#'   data = calibration$ds,
#'   xlab = "Off-axis angle (rad)",
#'   ylab = "Relative response"
#' )
#' angle <- seq(0, max(measurements$off_axis), length.out = 100)
#' response <- calc_relative_response(angle,
#'                                    "simple_polynomial",
#'                                    calibration$vignetting_coef,
#'                                    units = "radian")
#' lines(angle, response)
#' }
calibrate_radiometry <- function(measurements, degree = 3) {
  .check_columns(measurements, required_columns = c("off_axis", "dn"),
                  reserved_columns = c("reference_dn", "relative_response"))
  .check_vector(measurements$off_axis, "numeric", name = "measurements$off_axis")
  .check_vector(measurements$dn, "numeric", name = "measurements$dn")
  .check_vector(degree, "integerish", 1, sign = "positive")
  if (degree > 6) {
    stop("`degree` must not exceed 6 for simple_polynomial.", call. = FALSE)
  }
  if (nrow(measurements) == 0) {
    stop("`measurements` must contain at least one measurement.", call. = FALSE)
  }
  if (any(measurements$off_axis < 0 | measurements$off_axis > pi)) {
    stop("`off_axis` must lie in [0, pi] radians.", call. = FALSE)
  }
  ds <- measurements
  if (!"group" %in% names(ds)) ds$group <- rep("1", nrow(ds))
  if (!(is.character(ds$group) || is.factor(ds$group) || is.numeric(ds$group)) ||
      anyNA(ds$group) || any(!nzchar(as.character(ds$group)))) {
    stop("`group` must contain non-missing, non-empty group labels.",
         call. = FALSE)
  }
  ds$reference_dn <- numeric(nrow(ds))
  for (group in unique(ds$group)) {
    rows <- which(ds$group == group)
    angle <- ds$off_axis[rows]
    dn <- abs(ds$dn[rows])
    profile <- stats::aggregate(
      list(dn = rep(dn, 2)), list(off_axis = c(angle, -angle)), mean
    )
    reference <- if (nrow(profile) == 1) profile$dn else {
      stats::splinefun(profile$off_axis, profile$dn, method = "fmm")(0)
    }
    if (!is.finite(reference) || reference <= 0) {
      stop("The optical-axis reference must be finite and positive in group `",
           group, "`.", call. = FALSE)
    }
    ds$reference_dn[rows] <- reference
  }
  ds$relative_response <- abs(ds$dn) / ds$reference_dn
  fitted <- .fit_relative_response(ds, degree)
  c(list(ds = ds), fitted)
}

.fit_relative_response <- function(ds, degree) {
  .check_vector(degree, "integerish", 1, sign = "positive")
  if (degree > 6)
    stop("`degree` must not exceed 6 for simple_polynomial.", call. = FALSE)
  fit <- stats::lm(
    (1 - relative_response) ~ stats::poly(off_axis, degree, raw = TRUE) - 1,
    data = ds, na.action = stats::na.fail
  )
  coefficients <- -unname(stats::coef(fit))
  if (fit$rank < degree || any(!is.finite(coefficients))) {
    stop("Measurements do not support the requested polynomial degree.",
         call. = FALSE)
  }
  list(model = fit, vignetting_coef = coefficients)
}
