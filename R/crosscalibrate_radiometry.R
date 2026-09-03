#' Cross-calibrate radiometry
#'
#' @description
#' Derive a radiometric calibration model for an uncalibrated image by
#' referencing a radiometrically calibrated image taken under comparable
#' conditions.
#'
#' @param caim_uncal numeric [terra::SpatRaster-class] of one layer.
#' @param caim_cal numeric [terra::SpatRaster-class] of one layer.
#' @param degree integer-like vector of length one. Polynomial model degree.
#' @param rings [terra::SpatRaster-class] generated with [rcaiman::ring_segmentation()].
#'
#' @inheritParams rcaiman::compute_canopy_openness
#' @inheritParams rcaiman::add_directions
#'
#' @note
#' The experimental setup assumes an open sky under stable illumination
#' conditions. Overcast skies can be more variable than they appear, so
#' clear-sky conditions may be preferable. Direct sunlight must not enter the
#' lens. Argument `m` can be used to exclude abrupt radiance gradients or
#' regions that should not contribute to the cross-calibration, such as the
#' vicinity of the sun disk or areas of sky obstructed by canopy or buildings.
#'
#' @return List with named elements:
#' \describe{
#'   \item{`ds`}{data frame used to fit the model, with `theta` in radians and
#'     `radiometry` as the ratio between the normalized uncalibrated and
#'     calibrated ring profiles.}
#'   \item{`model`}{`lm` object fitted to `1 - radiometry` as a polynomial
#'     function of `theta`, without an intercept.}
#'   \item{`vignetting_coef`}{numeric vector of polynomial coefficients derived
#'     from `model`, suitable for `model = "ff_simple_polynomial"` in
#'     [add_radiometry_spec()].}
#'   \item{`profile_uncal`}{data frame with the normalized zenith-ring profile
#'     extracted from the uncalibrated image.}
#'   \item{`profile_cal`}{data frame with the normalized zenith-ring profile
#'     extracted from the calibrated reference image.}
#' }
#'
#' @seealso [calibrate_radiometry()], [add_radiometry_spec()]
#'
#' @export
crosscalibrate_radiometry <- function(r_uncal,
                                      r_cal,
                                      z,
                                      m,
                                      rings,
                                      degree = 5) {

  .assert_single_layer(r_uncal, squared = TRUE)
  .assert_single_layer(r_cal, squared = TRUE)
  .assert_zenith_image(z)
  .assert_logical_mask(m, squared = TRUE)
  .assert_single_layer(rings, squared = TRUE)
  return <- attr(rings, "return")
  if (is.null(return) || !is.character(return)) {
    stop("`return` attribute of argument `rings` is missing or not character.", call. = FALSE)
  } else if (return != "angle") {
    stop("Invalid `return` attribute of argument `rings`.", call. = FALSE)
  }
  .assert_same_geom(r_uncal, r_cal)
  .assert_same_geom(r_cal, z)
  .assert_same_geom(z, m)
  .assert_same_geom(z, rings)
  .check_vector(degree, "integerish", 1, sign = "positive")

  .extract_ring_profile <- function(r, rings) {
    profile <- compute_mean(r, rings * 1000, #because labels are forced to integer
                            return = "vector", ignore_label_0 = TRUE)
    profile <- data.frame(
      theta = theta <- as.numeric(names(profile)) / 1000 * pi / 180,
      radiometry = unname(profile)
    )
    profile <- profile[is.finite(profile$theta)
                       & is.finite(profile$radiometry)
                       & profile$radiometry > 0, ]
    profile <- profile[order(profile$theta), ]
    if (nrow(profile) == 0) {
      stop("Could not extract a valid ring profile.", call. = FALSE)
    }
    profile$radiometry <- profile$radiometry / profile$radiometry[1]
    profile
  }

  rings[m == 0] <- 0

  profile_uncal <- .extract_ring_profile(r_uncal, rings)
  profile_cal <- .extract_ring_profile(r_cal, rings)

  ds <- merge(
    profile_uncal,
    profile_cal,
    by = "theta",
    suffixes = c("_uncal", "_cal")
  )
  ds$radiometry <- ds$radiometry_uncal / ds$radiometry_cal
  ds <- ds[, c("theta", "radiometry")]

  fit <- stats::lm(
    (1 - radiometry) ~ stats::poly(theta, degree, raw = TRUE) - 1,
    data = ds
  )
  vignetting_coef <- -stats::coefficients(fit)

  list(
    ds = ds,
    model = fit,
    vignetting_coef = unname(vignetting_coef),
    profile_uncal = profile_uncal,
    profile_cal = profile_cal
  )
}

