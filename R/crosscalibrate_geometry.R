#' Cross-calibrate geometry
#'
#' @description
#' Fit a lens projection using corresponding points in both an uncalibrated image
#' and an image with known internal viewing directions.
#'
#' @section Acquisition:
#'
#' The user is responsible for acquiring the two photographs
#' with matching entrance-pupil positions, aligned optical axes, and the tops
#' of both sensors oriented identically. These conditions are assumed by
#' the fit. The function does not detect, estimate, or compensate for
#' misalignment or other acquisition errors.
#'
#' Corresponding points must be digitized in the same order. The split-view
#' feature in
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects)
#' facilitates this task. The fitted coefficients and residuals combine
#' experimental error with model approximation error. Experimental error can
#' also be absorbed into the coefficients, so residuals alone do not measure
#' calibration accuracy.
#'
#' This method allows several imaging systems to share a calibrated reference.
#' See [calibrate_geometry()] for direct calibration with the simple method.
#'
#' @section Reference directions:
#'
#' Prepare the reference geometry with [rcaiman::set_internal_geometry()].
#' Convert its digitized coordinates to pixel indices with
#' [rcaiman::indices_from_coord()] and obtain internal directions with
#' [rcaiman::add_directions()]. Supply the resulting table as `pts_cal`. The
#' reference can use any model supported by `set_internal_geometry()`. Reference
#' directions describe the selected pixel centers. Coordinates in `pts_uncal`
#' remain continuous, without rounding to pixel indices.
#'
#' @section Radial projection:
#'
#' With `model = "radial_projection"`, radius in pixels is fitted as a
#' polynomial of `off_axis` in radians, without an intercept. Choose the
#' definitive, integer-coordinate `center_of_vision` before fitting and use
#' the same center in the resulting [geometry_spec]. The function uses the
#' supplied center unchanged; it does not estimate or snap it.
#'
#' All paired observations enter the fit with equal weight. Any exclusions
#' must be made explicitly in both tables, preserving their correspondence.
#' Missing directions are not silently removed. The returned [stats::lm]
#' predicts pixel radius from `off_axis` in radians, matching the units of
#' `add_directions()`.
#'
#' The fitted radius at `normalization_angle` defines `normalization_radius`.
#' Dividing the fitted coefficients by this radius gives `projection_coef`.
#' Changing the normalization preserves the fitted projection in pixels.
#' A reference outside the sampled angular range involves extrapolation.
#' Inspect the fit before constructing a [geometry_spec], where monotonicity
#' over the chosen calculation interval is checked. The fit does not infer
#' `max_observed_off_axis_angle` or choose `max_off_axis_angle`.
#'
#' @section Anisotropic projection:
#'
#' With `model = "anisotropic_projection"`, the reference table supplies
#' `off_axis` and `internal_azimuth` in radians. The definitive integer center
#' is used by [rcaiman::add_image_polar()] to prepare the uncalibrated points.
#' Distances from this center are divided by the image semidiagonal and
#' transformed as `atan(relative_radius / atan_scale)`. Image-plane angle
#' starts at the right and increases clockwise; internal azimuth starts at
#' the top of the sensor and increases clockwise.
#'
#' Three models are fitted without intercepts: a polynomial for `off_axis`,
#' and first-harmonic regressions for `sin(internal_azimuth)` and
#' `cos(internal_azimuth)`. Internal azimuth is recovered with `atan2(sin, cos)`.
#' All observations have equal weight and their pairing is retained, as in
#' the radial fit. The equations and parameter convention are documented in
#' [rcaiman::set_internal_geometry()].
#'
#' Supply the returned `parameters` and unchanged `center_of_vision` to
#' [add_geometry_spec()]. The image dimensions used there must match
#' `width_uncal` and `height_uncal`: they define the semidiagonal used during
#' fitting and evaluation. `atan_scale` is supplied by the user, not fitted.
#' This model does not use `normalization_angle`.
#'
#' @param pts_uncal data frame with finite numeric columns `x` and `y_down`:
#'   continuous image coordinates of digitized points in the uncalibrated
#'   image. Additional columns are preserved in `ds`. For a radial fit,
#'   `off_axis` and `radius` are reserved column names.
#' @param pts_cal data frame of corresponding reference directions. For a
#'   radial fit, the finite numeric column `off_axis` in radians (`[0, pi]`)
#'   is required, normally supplied by [rcaiman::add_directions()]. Other
#'   columns are ignored. The anisotropic fit also requires finite numeric
#'   `internal_azimuth` in radians (`[0, 2 * pi)`). Rows are paired by position with
#'   `pts_uncal`; both tables must have the same number of rows.
#' @param model character vector of length one. Model to fit:
#'   `"radial_projection"` or `"anisotropic_projection"`.
#' @param center_of_vision integer-like numeric vector of length two.
#'   Definitive optical center `c(x, y_down)` in the uncalibrated image,
#'   measured from its upper-left corner. Integer coordinates lie on pixel
#'   boundaries. The center need not lie inside the image. Required for both
#'   models and used unchanged.
#' @param degree positive integer-like numeric vector of length one.
#'   Polynomial degree, default 3. The observations must support estimation
#'   of every coefficient in each fitted model.
#' @param normalization_angle positive numeric vector of length one, at most
#'   180. Off-axis angle in degrees defining the radial normalization
#'   reference, default 90. Its fitted radius must be finite and positive.
#'   Unused by the anisotropic model.
#' @param width_uncal,height_uncal integer-like numeric vectors of length one.
#'   Uncalibrated image dimensions, required only for the anisotropic
#'   branch and passed to [rcaiman::add_image_polar()].
#' @param atan_scale positive numeric vector of length one. Scale used in
#'   `atan(relative_radius / atan_scale)`, required only for the
#'   anisotropic model.
#'
#' @return For `model = "radial_projection"`, a list containing:
#' \describe{
#'   \item{`ds`}{Uncalibrated point coordinates and additional columns in their
#'     original order, with transferred `off_axis` in radians and measured
#'     `radius` in pixels.}
#'   \item{`model`}{Fitted [stats::lm] predicting pixel radius from `off_axis`
#'     in radians. Its residuals are observed minus fitted radii in pixels,
#'     available with [stats::residuals()]. They combine experimental and
#'     model approximation errors.}
#'   \item{`normalization_radius`}{Fitted radius in pixels at the reference.}
#'   \item{`normalization_angle`}{Normalization reference in degrees.}
#'   \item{`projection_coef`}{Coefficients in increasing order of degree,
#'     without an intercept, evaluated in radians and normalized at the
#'     reference. Supply as `parameters` in a radial [geometry_spec].}
#'   \item{`center_of_vision`}{The supplied center, unchanged.}
#' }
#'
#' For `model = "anisotropic_projection"`, the list contains:
#' \describe{
#'   \item{`ds`}{Original coordinates and additional columns, with
#'     `relative_radius`, `image_angle`, `atan_radius`, transferred `off_axis`
#'     and `internal_azimuth`, `fitted_off_axis`, `fitted_internal_azimuth`,
#'     `off_axis_residual`, `internal_azimuth_residual`, and `angular_error`.
#'     Angles and angular residuals are in radians. Residuals are observed
#'     minus fitted; the azimuth residual is wrapped to `[-pi, pi]`.
#'     `angular_error` is the unsigned spherical separation. Fitted azimuth
#'     and its diagnostics are `NA` at the exact center or when both fitted
#'     sine and cosine are zero, as in the core. These derived column names
#'     are reserved in `pts_uncal`.}
#'   \item{`model`}{Three fitted `lm` objects named `off_axis`,
#'     `sin_internal_azimuth`, and `cos_internal_azimuth`.}
#'   \item{`parameters`}{List with `off_axis_coef`, `sin_internal_azimuth_coef`,
#'     `cos_internal_azimuth_coef`, and `atan_scale`, following the contract of
#'     [rcaiman::set_internal_geometry()].}
#'   \item{`center_of_vision`}{The supplied center, unchanged.}
#' }
#'
#' @seealso [calibrate_geometry()], [calc_center_of_vision()],
#'   [rcaiman::normalize_projection_coef()]
#' @export
#' @examples
#' \dontrun{
#' # Read corresponding points digitized in the original image grids.
#' pts_cal <- read_points("points_reference.csv")
#' pts_uncal <- read_points("points_uncalibrated.csv")
#' ig_cal <- set_internal_geometry(reference_dim, reference_geometry)
#' indices <- indices_from_coord(pts_cal$x, pts_cal$y_down)
#' directions <- add_directions(indices, internal_geometry = ig_cal)
#'
#' # Estimate the center from a separate point layer on a circular feature.
#' center_points <- read_points("points_center.csv")
#' estimated_center <- calc_center_of_vision(center_points)
#' center <- round(estimated_center)
#' calibration <- crosscalibrate_geometry(
#'   pts_uncal,
#'   directions,
#'   model = "radial_projection",
#'   center_of_vision = center,
#'   normalization_angle = 60
#' )
#' plot(radius ~ off_axis,
#'      data = calibration$ds,
#'      xlab = "Off-axis angle (rad)",
#'      ylab = "Radius (pixels)")
#' angle <- seq(0, max(calibration$ds$off_axis), length.out = 100)
#' lines(angle, predict(calibration$model, data.frame(off_axis = angle)))
#' entry <- add_geometry_spec(
#'   entry,
#'   "raw",
#'   "bands",
#'   "crosscalibration",
#'   model = "radial_projection",
#'   parameters = calibration$projection_coef,
#'   center_of_vision = calibration$center_of_vision,
#'   normalization_radius = calibration$normalization_radius,
#'   normalization_angle = calibration$normalization_angle,
#'   max_off_axis_angle = 90
#' )
#' geometry <- get_geometry_spec(entry, "raw", "bands", "crosscalibration")
#' ig <- set_internal_geometry(uncalibrated_dim, geometry)
#'
#' # Alternatively, fit the anisotropic model to the same paired points.
#' calibration <- crosscalibrate_geometry(
#'   pts_uncal,
#'   directions,
#'   model = "anisotropic_projection",
#'   center_of_vision = center,
#'   width_uncal = uncalibrated_dim[1],
#'   height_uncal = uncalibrated_dim[2],
#'   atan_scale = 0.6
#' )
#' entry <- add_geometry_spec(
#'   entry,
#'   "raw",
#'   "bands",
#'   "crosscalibration_anisotropic",
#'   model = "anisotropic_projection",
#'   parameters = calibration$parameters,
#'   center_of_vision = calibration$center_of_vision
#' )
#'
#' geometry <- get_geometry_spec(entry, "raw", "bands", "crosscalibration_anisotropic")
#' ig <- set_internal_geometry(uncalibrated_dim, geometry)
#' calibration$ds$angular_error
#' }
crosscalibrate_geometry <- function(pts_uncal,
                                    pts_cal,
                                    model,
                                    center_of_vision = NULL,
                                    degree = 3,
                                    normalization_angle = 90,
                                    width_uncal = NULL,
                                    height_uncal = NULL,
                                    atan_scale = NULL) {
  if (missing(model)) {
    stop("`model` must be supplied.", call. = FALSE)
  }
  .check_vector(model, "character", 1)
  .assert_choice(model, c("radial_projection", "anisotropic_projection"))
  .check_points_coord(pts_uncal, "pts_uncal")
  .check_columns(pts_cal, required_columns =
    if (model == "radial_projection") "off_axis" else c("off_axis", "internal_azimuth"))
  if (nrow(pts_uncal) != nrow(pts_cal)) {
    stop("`pts_uncal` and `pts_cal` must have the same number of rows.",
         call. = FALSE)
  }
  .check_vector(degree, "integerish", 1, sign = "positive")
  .check_vector(center_of_vision, "integerish", 2)
  .check_vector(pts_cal$off_axis, "numeric", sign = "nonnegative",
                name = "pts_cal$off_axis")
  if (any(pts_cal$off_axis > pi))
    stop("`pts_cal$off_axis` must not exceed pi radians.", call. = FALSE)

  if (model == "radial_projection") {
    .check_columns(pts_uncal, required_columns = c("x", "y_down"),
                   reserved_columns = c("off_axis", "radius"))
    .check_vector(normalization_angle, "numeric", 1, sign = "positive")
    if (normalization_angle > 180)
      stop("`normalization_angle` must not exceed 180 degrees.", call. = FALSE)
    if (nrow(pts_uncal) < degree ||
        length(unique(pts_cal$off_axis[pts_cal$off_axis > 0])) < degree)
      stop("Corresponding points do not support the requested polynomial degree.",
           call. = FALSE)

    ds <- pts_uncal
    ds$off_axis <- pts_cal$off_axis
    ds$radius <- sqrt((ds$x - center_of_vision[1])^2 +
                       (ds$y_down - center_of_vision[2])^2)
    .check_vector(ds$radius, "numeric", name = "radius")
    fit <- stats::lm(radius ~ poly(off_axis, degree, raw = TRUE) - 1, data = ds,
                     na.action = stats::na.fail)
    coefficients <- unname(stats::coef(fit))
    if (fit$rank < degree || any(!is.finite(coefficients)))
      stop("Corresponding points do not support the requested polynomial degree.",
           call. = FALSE)
    normalization_radius <- rcaiman::calc_relative_radius(
      normalization_angle, coefficients
    )
    projection_coef <- rcaiman::normalize_projection_coef(
      coefficients, normalization_angle
    )
    return(list(ds = ds,
                model = fit,
                normalization_radius = unname(normalization_radius),
                normalization_angle = normalization_angle,
                projection_coef = projection_coef,
                center_of_vision = center_of_vision))
  }

  .check_columns(pts_uncal, required_columns = c("x", "y_down"),
    reserved_columns = c("relative_radius", "image_angle", "atan_radius",
      "off_axis", "internal_azimuth", "fitted_off_axis", "fitted_internal_azimuth",
      "off_axis_residual", "internal_azimuth_residual", "angular_error"))
  .check_vector(pts_cal$internal_azimuth, "numeric", sign = "nonnegative",
                name = "pts_cal$internal_azimuth")
  if (any(pts_cal$internal_azimuth >= 2 * pi))
    stop("`pts_cal$internal_azimuth` must be less than 2 * pi radians.", call. = FALSE)
  .check_vector(atan_scale, "numeric", 1, sign = "positive")

  ds <- rcaiman::add_image_polar(pts_uncal, width = width_uncal,
    height = height_uncal, center_of_vision = center_of_vision)
  ds$off_axis <- pts_cal$off_axis
  ds$internal_azimuth <- pts_cal$internal_azimuth
  ds$atan_radius <- atan(ds$relative_radius / atan_scale)
  if (nrow(ds) < max(degree, 2) ||
      length(unique(ds$atan_radius[ds$atan_radius > 0])) < degree)
    stop("Corresponding points do not support the requested polynomial degree.",
         call. = FALSE)

  models <- list(
    off_axis = stats::lm(off_axis ~ poly(atan_radius, degree, raw = TRUE) - 1,
                         data = ds, na.action = stats::na.fail),
    sin_internal_azimuth = stats::lm(
      sin(internal_azimuth) ~ sin(image_angle) + cos(image_angle) - 1,
      data = ds, na.action = stats::na.fail),
    cos_internal_azimuth = stats::lm(
      cos(internal_azimuth) ~ sin(image_angle) + cos(image_angle) - 1,
      data = ds, na.action = stats::na.fail)
  )
  ranks <- c(degree, 2, 2)
  for (i in seq_along(models)) {
    if (models[[i]]$rank < ranks[i] || any(!is.finite(stats::coef(models[[i]]))))
      stop("Corresponding points do not support the ", names(models)[i],
           " model.", call. = FALSE)
  }
  parameters <- list(
    off_axis_coef = unname(stats::coef(models$off_axis)),
    sin_internal_azimuth_coef = unname(stats::coef(models$sin_internal_azimuth)),
    cos_internal_azimuth_coef = unname(stats::coef(models$cos_internal_azimuth)),
    atan_scale = atan_scale
  )
  rcaiman::.check_internal_geometry(list(model = model, parameters = parameters,
                                         center_of_vision = center_of_vision))

  ds$fitted_off_axis <- unname(stats::predict(models$off_axis, ds))
  s <- unname(stats::predict(models$sin_internal_azimuth, ds))
  c <- unname(stats::predict(models$cos_internal_azimuth, ds))
  ds$fitted_internal_azimuth <- atan2(s, c) %% (2 * pi)
  ds$fitted_internal_azimuth[ds$fitted_internal_azimuth == 2 * pi] <- 0
  ds$fitted_internal_azimuth[ds$relative_radius == 0 | (s == 0 & c == 0)] <- NA_real_
  ds$off_axis_residual <- ds$off_axis - ds$fitted_off_axis
  difference <- ds$internal_azimuth - ds$fitted_internal_azimuth
  ds$internal_azimuth_residual <- atan2(sin(difference), cos(difference))
  ds$angular_error <- rcaiman::calc_spherical_distance(
    ds$off_axis, ds$internal_azimuth, ds$fitted_off_axis, ds$fitted_internal_azimuth)

  list(ds = ds, model = models, parameters = parameters,
       center_of_vision = center_of_vision)
}
