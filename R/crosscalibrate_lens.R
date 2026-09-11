#' Cross-calibrate geometry
#'
#' @description
#' Estimate a lens projection for an uncalibrated camera by referencing a
#' calibrated camera photographed from the same location.
#'
#' @details
#' The calibrated and uncalibrated photographs must be taken with matching
#' entrance pupils and aligned optical axes. Corresponding points digitized in
#' both images are used to transfer viewing directions from the calibrated
#' image to the uncalibrated image and fit its geometry model.
#'
#' This method is intended for cases where a camera calibrated with a method of
#' higher accuracy than [calibrate_geometry()] is available, or when several
#' cameras should be cross-calibrated against a common reference camera.
#'
#' Points must be digitized in tandem. The split-view feature in
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects)
#' facilitates this task. See [calibrate_geometry()] for conceptual background.
#'
#' `model` identifies the model fitted for the uncalibrated camera. The
#' `projection_coef` and `diameter_cal` arguments instead describe the known
#' geometry of the calibrated camera.
#'
#' With `model = "radial_projection"`, the function preserves the historical
#' radial cross-calibration: zenith angles are transferred from the calibrated
#' radial projection and radial distances are fitted for the uncalibrated
#' camera.
#'
#' With `model = "anisotropic_projection"`, `pts_cal` supplies the known
#' directions corresponding to `pts_uncal`. The uncalibrated coordinates are
#' prepared with [rcaiman::add_image_polar()] and three models are fitted:
#' `zen` as a polynomial of transformed relative radius, and `sin(azi)` and
#' `cos(azi)` as first-harmonic functions of image-plane angle. The resulting
#' parameters follow the contract of [rcaiman::add_directions_from_model()].
#' The zenith component remains a function of radius alone; anisotropy refers
#' to the fitted angular mapping rather than to zenith variation with azimuth.
#'
#' @param pts_uncal,pts_cal `data.frame` objects with numeric columns `"x"` and
#'   `"y_down"`, giving continuous image coordinates of corresponding
#'   digitized points in the uncalibrated and calibrated images, respectively.
#'   Rows are paired by position and both objects must have the same number of
#'   rows. For `model = "anisotropic_projection"`, `pts_cal` must also contain
#'   numeric columns `"zen"` and `"azi"` in radians. Additional columns are
#'   ignored, except that additional columns in `pts_uncal` are preserved in
#'   the returned calibration data.
#' @param model character vector of length one. Model to fit for the
#'   uncalibrated camera. Either `"radial_projection"` or
#'   `"anisotropic_projection"`.
#' @param zenith_coord_uncal,zenith_coord_cal numeric vectors of length two.
#'   Continuous image coordinates `c(x, y_down)` of the zenith in the
#'   uncalibrated and calibrated images, respectively. See
#'   [calc_zenith_coord()]. Required only for `"radial_projection"`.
#' @param diameter_cal numeric vector of length one. Image diameter (pixels) of
#'   the calibrated camera. Required only for `"radial_projection"`.
#' @param projection_coef numeric vector. Lens projection coefficients of the
#'   calibrated camera. Required only for `"radial_projection"`.
#' @param degree numeric vector of length one. Polynomial degree used to fit the
#'   lens projection of the uncalibrated camera (default 3).
#' @param width_uncal,height_uncal integer-like numeric vectors of length one.
#'   Width and height in pixels of the uncalibrated image. Required only for
#'   `"anisotropic_projection"` and passed to [rcaiman::add_image_polar()].
#' @param atan_scale positive numeric vector of length one. Scale used in
#'   `atan(relative_radius / atan_scale)`. Required only for
#'   `"anisotropic_projection"`.
#'
#' @return For `model = "radial_projection"`, a list with the following
#' components:
#' \describe{
#'   \item{`ds`}{`data.frame` with zenith angle (`theta`, radians) and radial
#'     distance (`px`, pixels) for the uncalibrated camera.}
#'   \item{`model`}{`lm` object fitted to radial distance as a polynomial
#'     function of zenith angle. This is the fitted statistical model, not the
#'     model identifier supplied in the `model` argument.}
#'   \item{`horizon_radius`}{numeric vector of length one. Predicted radial
#'     distance (pixels) at 90 deg.}
#'   \item{`projection_coef`}{numeric vector. Polynomial coefficients normalized
#'     by `horizon_radius`.}
#' }
#'
#' For `model = "anisotropic_projection"`, a list with the following
#' components:
#' \describe{
#'   \item{`ds`}{`data.frame` containing the uncalibrated continuous image
#'     coordinates, image-centered polar predictors, `atan_radius`, and the
#'     transferred `zen` and `azi` directions.}
#'   \item{`model`}{Named list containing the fitted `lm` objects `zen`,
#'     `azi_sin`, and `azi_cos`.}
#'   \item{`parameters`}{Named list following the anisotropic projection
#'     contract documented in [rcaiman::add_directions_from_model()].}
#' }
#'
#' @seealso [calibrate_geometry()], [calc_zenith_coord()]
#'
#' @export
# Correction record (2026-09-11): the data-frame migration left obsolete CSV
# reads in this function. The historical radial fitting algorithm is preserved;
# only its input contract is migrated to continuous image coordinates.
# Design record (2026-09-11): the anisotropic branch reproduces the forward
# fitting path validated in data-raw/proyectar_pan.R. Directions are supplied
# by pts_cal; no inverse anisotropic transformation is introduced here.
crosscalibrate_geometry <- function(pts_uncal,
                                    pts_cal,
                                    model,
                                    zenith_coord_uncal = NULL,
                                    zenith_coord_cal = NULL,
                                    diameter_cal = NULL,
                                    projection_coef = NULL,
                                    degree = 3,
                                    width_uncal = NULL,
                                    height_uncal = NULL,
                                    atan_scale = NULL) {

  if (missing(model)) {
    stop("`model` must be supplied.", call. = FALSE)
  }
  .assert_choice(
    model,
    c("radial_projection", "anisotropic_projection")
  )
  .check_points_coord(pts_uncal, "pts_uncal")
  .check_points_coord(pts_cal, "pts_cal")
  if (nrow(pts_uncal) != nrow(pts_cal)) {
    stop(
      "`pts_uncal` and `pts_cal` must have the same number of rows.",
      call. = FALSE
    )
  }
  .check_vector(degree, "integerish", 1, sign = "positive")

  if (model == "radial_projection") {
    .check_vector(zenith_coord_uncal, "numeric", 2, sign = "positive")
    .check_vector(zenith_coord_cal, "numeric", 2, sign = "positive")
    .check_vector(diameter_cal, "numeric", 1, sign = "positive")
    .check_vector(projection_coef, "numeric", sign = "any")

    coord_uncal <- as.matrix(pts_uncal[c("x", "y_down")])
    coord_cal <- as.matrix(pts_cal[c("x", "y_down")])

    # Center continuous image coordinates on their respective zeniths. The
    # downward y axis does not change radial distance.
    coord_uncal[, 1] <- coord_uncal[, 1] - zenith_coord_uncal[1]
    coord_uncal[, 2] <- coord_uncal[, 2] - zenith_coord_uncal[2]
    coord_cal[, 1] <- coord_cal[, 1] - zenith_coord_cal[1]
    coord_cal[, 2] <- coord_cal[, 2] - zenith_coord_cal[2]

    polar_uncal <- geometry::cart2pol(coord_uncal)
    polar_cal <- geometry::cart2pol(coord_cal)
    px <- polar_uncal[, "r"]

    angle <- seq(0, 90, length.out = 90)
    R <- calc_relative_radius(angle, projection_coef)
    inv_fun <- splinefun(R * diameter_cal/2, angle * pi / 180)

    theta <- inv_fun(polar_cal[, "r"])

    fit <- lm(px ~ poly(theta, degree, raw = TRUE) - 1)
    horizon_radius <- stats::predict(
      fit,
      data.frame(theta = pi / 2)
    ) %>% unname()

    return(list(
      ds = data.frame(theta, px),
      model = fit,
      horizon_radius = horizon_radius,
      projection_coef = (coefficients(fit) / horizon_radius) %>% unname()
    ))
  }

  .check_columns(
    pts_uncal,
    required_columns = c("x", "y_down"),
    reserved_columns = c(
      "relative_radius", "image_angle", "atan_radius", "zen", "azi"
    )
  )
  .check_columns(
    pts_cal,
    required_columns = c("x", "y_down", "zen", "azi")
  )
  .check_vector(pts_cal$zen, "numeric", sign = "nonnegative")
  .check_vector(pts_cal$azi, "numeric", sign = "nonnegative")
  if (any(pts_cal$zen > pi)) {
    stop("`pts_cal$zen` cannot exceed `pi` radians.", call. = FALSE)
  }
  if (any(pts_cal$azi > 2 * pi)) {
    stop("`pts_cal$azi` cannot exceed `2 * pi` radians.", call. = FALSE)
  }
  .check_vector(atan_scale, "numeric", 1, sign = "positive")

  ds <- rcaiman::add_image_polar(
    pts_uncal,
    width = width_uncal,
    height = height_uncal
  )
  ds$zen <- pts_cal$zen
  ds$azi <- pts_cal$azi
  ds$atan_radius <- atan(ds$relative_radius / atan_scale)

  models <- list(
    zen = stats::lm(
      zen ~ poly(atan_radius, degree, raw = TRUE) - 1,
      data = ds
    ),
    azi_sin = stats::lm(
      sin(azi) ~ sin(image_angle) + cos(image_angle) - 1,
      data = ds
    ),
    azi_cos = stats::lm(
      cos(azi) ~ sin(image_angle) + cos(image_angle) - 1,
      data = ds
    )
  )

  parameters <- list(
    zen_coef = unname(stats::coef(models$zen)),
    azi_sin_coef = unname(stats::coef(models$azi_sin)),
    azi_cos_coef = unname(stats::coef(models$azi_cos)),
    atan_scale = atan_scale
  )
  .assert_anisotropic_projection_parameters(parameters)

  list(ds = ds, model = models, parameters = parameters)
}
