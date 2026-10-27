#' Calibrate a radial projection with the simple method
#'
#' @description
#' Fit a radial projection from image coordinates of pins at known off-axis
#' angles.
#'
#' @details
#' The simple method described by
#' \insertCite{Diaz2024;textual}{rcaiman.registry} relates radial distance in
#' pixels to off-axis angle. It assumes circular symmetry, as described for
#' the radial model in [rcaiman::set_internal_geometry()].
#'
#' Each image provides a continuous center-of-vision estimate from its first
#' pin. Distances are measured relative to that image's center. The observations
#' from all images are pooled to fit a polynomial without an intercept, using
#' angles in radians. The returned center is the coordinate-wise median of
#' the individual centers. All images must use the same image coordinate frame.
#'
#' The fitted radius at `normalization_angle` defines the scale. Dividing the
#' polynomial coefficients by this radius gives `projection_coef`; changing
#' the reference preserves the fitted projection in pixels. Normalization
#' outside the range of the digitized pins involves extrapolation.
#'
#' The returned [stats::lm] model predicts pixel radius from `off_axis` supplied
#' in radians, as in [crosscalibrate_geometry()]. Inspect the fit before
#' constructing a [geometry_spec], where normalization and monotonicity over
#' the chosen calculation interval are checked. A separately measured perimeter
#' radius can be divided by `normalization_radius` and converted to an
#' estimated off-axis angle with [rcaiman::calc_off_axis_angle()].
#'
#' @section Step-by-step guide for producing a CSV file to feed this function:
#'
#' ### Materials
#'
#' * this package and [ImageJ](https://imagej.net/ij/download.html)
#' * camera and lens
#' * tripod
#' * standard yoga mat
#' * table at least as wide as the yoga mat width
#' * twenty two push pins of different colors
#' * one print of this [sheet](https://osf.io/tudzc) (A1 size,
#' almost like a research poster).
#' * scissors
#' * some patience
#'
#' ### Instructions
#'
#' Cut the sheet by the dashed line. Place the yoga mat extended on top of the
#' table. Place the sheet on top of the yoga mat. Align the dashed line with the
#' yoga mat border closest to you. Place push pins on each cross. If you are
#' gentle, the yoga mat will allow you to do that without damaging the table. Of
#' course, other materials could be used to obtain the same result, such as
#' cardboard, foam, nails, etc.
#' \if{html}{\figure{calibrationBoard.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{calibrationBoard.jpg}}
#' Place the camera on the tripod. Align its optical axis with the table while
#' looking for getting an image showing the overlapping of the three pairs of
#' push pins, as instructed in the print. In order to take care of the line of
#' pins at 90 deg relative to the optical axis, it may be of help to use the naked
#' eye to align the entrance pupil of the lens with the pins. The alignment of
#' the push pins only guarantees the position of the lens entrance pupil, the
#' leveling should be checked with an instrument, and the alignment between the
#' optical axis and the radial line through the optical-axis push pin should be taken into
#' account. In practice, the latter is achieved by aligning the camera body with
#' the orthogonal frame made by the quarter circle.
#'
#' Take a photo and transfer it to the computer. Use
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects) to
#' create a point layer. This creates a csv that you can read with
#' [rcaiman::read_points()]. Digitize the photographed push pins, starting
#' from the one on the optical axis (0 deg), then proceeding at 5 deg intervals
#' without skipping a pin. Stop at the last visible pin, up to 90 deg. Every
#' point must identify a pin.
#'
#' \if{html}{\figure{pushpinsImageJ.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{pushpinsImageJ.jpg}}
#'
#' @note
#' To calibrate different directions, think of the fisheye image as an analog
#' clock. To calibrate 3 o'clock, attach the camera to the tripod in landscape
#' mode while leaving the quarter-circle at the lens's right side. To calibrate
#' 9 o'clock, rotate the camera to put the quarter-circle at the lens's left
#' side. To calibrate 12 and 6 o'clock, do the same but with the camera in
#' portrait mode.
#'
#' @section Rescaling an existing radial calibration:
#'
#' A previously calibrated radial projection can be reused by estimating its
#' scale in the current image while keeping the shape of the projection curve.
#' This can be useful, for example, when remounting a clip-on lens on a
#' new smartphone. The calculation below uses [rcaiman::calc_relative_radius()]
#' directly; it does not require fitting a new polynomial with
#' `calibrate_geometry()`.
#'
#' ### Measuring radius-angle pairs
#'
#' The required measurements can be obtained following the
#' [Hemisfer user manual](https://www.schleppi.ch/patrick/hemisfer/help/en/lens.htm).
#' A simpler arrangement uses a vertical wall and a level floor:
#'
#' 1. Mark a 3-4-5 m right triangle on the floor, with the 4 m side along
#'    the wall. The 3 m side is perpendicular to the wall.
#' 2. Position the lens entrance pupil above the vertex 3 m from the wall,
#'    at a convenient height, such as 1.3 m.
#' 3. Mark the wall at that same height, directly opposite the entrance pupil.
#'    Add four marks at 1 m intervals along a horizontal line towards the other
#'    wall vertex.
#' 4. Point the optical axis horizontally and perpendicular to the wall, towards
#'    the first mark. In the image, this mark should coincide with the center
#'    of vision. This is a horizontal acquisition, irrespective of the
#'    orientation used later in the field.
#' 5. Measure the pixel radius from the center of vision to each of the other
#'    four marks. The 0 deg mark locates the center and is not used to estimate
#'    the scale.
#'
#' The four off-axis angles are `atan((1:4) / 3) * 180 / pi`, approximately
#' 18.43, 33.69, 45, and 53.13 deg.
#'
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects)
#' offers a measuring tool in View>Measure triangle. It can be used to measure
#' radius. The center of vision can be obtained [calc_center_of_vision()].
#'
#' ### Estimating the scale
#'
#' In this example, `geometry` is an existing radial [geometry_spec] or an
#' equivalent list accepted by [rcaiman::set_internal_geometry()]. Its
#' coefficients are normalized at `geometry$normalization_angle`. Replace
#' the illustrative radii with your measurements, in the same order as `angle`.
#'
#' ```r
#' angle <- atan((1:4) / 3) * 180 / pi
#' radius <- c(205, 374, 500, 590)
#' projection_coef <- geometry$parameters
#' relative_radius <- rcaiman::calc_relative_radius(angle, projection_coef)
#' estimated_radius <- radius / relative_radius
#' data.frame(angle, radius, estimated_radius)
#' normalization_radius <- median(estimated_radius)
#' ```
#'
#' Each measurement estimates the radius in pixels at the normalization angle.
#' Inspect these estimates before summarizing them. A systematic change with
#' angle suggests that a single scale does not explain the measurements.
#' The median provides one estimate of the scale; it is not rounded to an
#' integer and need not refer to 90 deg. The coefficients and their
#' normalization angle remain unchanged.
#'
#' ### Checking an existing or rescaled calibration
#'
#' Compare the measured radii with predictions from the existing scale and
#' from the newly estimated one:
#'
#' ```r
#' predicted_existing <- geometry$normalization_radius * relative_radius
#' predicted_rescaled <- normalization_radius * relative_radius
#' data.frame(angle, observed = radius,
#'            predicted_existing, predicted_rescaled,
#'            residual_existing = radius - predicted_existing,
#'            residual_rescaled = radius - predicted_rescaled)
#' ```
#'
#' The first comparison checks the existing calibration against the new
#' measurements. The second shows how well rescaling represents those same
#' measurements. After reviewing the result, use the estimated
#' `normalization_radius` when constructing the updated [geometry_spec].
#' The center of vision is a separate measurement and should also correspond
#' to the current image.
#'
#'
#' @param calibration_points_coord nonempty list of data frames with numeric
#'   columns `x` and `y_down`, in continuous image coordinates. Each frame
#'   contains 2 to 19 consecutive pin positions at 0, 5, ..., up to 90 deg.
#'   The first row identifies the pin on the optical axis. Every row is used
#'   in the fit.
#' @param degree positive integer-like numeric vector of length one.
#'   Polynomial degree. The pooled observations must support estimation of
#'   all coefficients.
#' @param normalization_angle positive numeric vector of length one, at most
#'   180. Off-axis angle in degrees defining the normalization reference.
#'   Defaults to 90. The fitted radius at this angle must be finite and positive.
#'
#' @return list with the following components:
#'   \describe{
#'     \item{`ds`}{Data frame with `off_axis` in radians and `radius` in pixels,
#'       containing all pin observations used in the fit.}
#'     \item{`model`}{Fitted [stats::lm] object predicting pixel radius from
#'       `off_axis` in radians.}
#'     \item{`normalization_radius`}{Continuous fitted radius in pixels at
#'       `normalization_angle`.}
#'     \item{`normalization_angle`}{Normalization reference in degrees.}
#'     \item{`projection_coef`}{Normalized polynomial coefficients, in
#'       increasing order of degree and evaluated in radians. Suitable for
#'       `parameters` in a radial [geometry_spec].}
#'     \item{`center_of_vision`}{Continuous coordinates `c(x, y_down)`. Review
#'       and snap to integer coordinates before supplying the center to
#'       [add_geometry_spec()].}
#'   }
#'
#' @references \insertAllCited{}
#'
#' @seealso [rcaiman::normalize_projection_coef()],
#'   [rcaiman::calc_relative_radius()], [crosscalibrate_geometry()]
#'
#' @export
#' @examples
#' path <- c(system.file("external/a_DSCN7172.csv", package = "rcaiman.registry"),
#'           system.file("external/b_DSCN7162.csv", package = "rcaiman.registry"))
#' calibration_points_coord <- lapply(path, read_points)
#' calibration <- calibrate_geometry(calibration_points_coord)
#' calibration$projection_coef
#' calibration$normalization_radius
#'
#'
#' plot(c(0, pi/2), range(calibration$ds$radius), type = "n",
#'      axes = FALSE,
#'      xlab = "Off-axis angle",
#'      ylab = "Fisheye image radius (pixel)"
#' )
#' box()
#' axis(1, labels = c(0,expression(pi/8), expression(pi/4),
#'                    expression(pi*3/8), expression(pi/2)),
#'      at = c(0, pi/8, pi/4, pi*3/8, pi/2))
#' axis(2)
#'
#' points(calibration$ds$off_axis,
#'        calibration$ds$radius,
#'        pch = 1,
#'        col = 1)
#'
#' off_axis <- seq(0, pi/2, pi/180)
#' lines(
#'   off_axis,
#'   calc_relative_radius(off_axis,
#'                        calibration$projection_coef,
#'                        units = "radian") * calibration$normalization_radius
#' )
#'
#' # Prepare an internal geometry after reviewing the fit.
#' geometry <- list(
#'   model = "radial_projection",
#'   parameters = calibration$projection_coef,
#'   center_of_vision = round(calibration$center_of_vision),
#'   normalization_radius = calibration$normalization_radius,
#'   normalization_angle = calibration$normalization_angle,
#'   max_off_axis_angle = 90
#' )
#' ig <- set_internal_geometry(c(1288, 962), geometry)
#'
#' # The same pixel projection with a different normalization reference.
#' calibration_60 <- calibrate_geometry(calibration_points_coord,
#'                                      normalization_angle = 60)
#' calibration_60$normalization_radius
calibrate_geometry <- function(calibration_points_coord, degree = 3,
                                normalization_angle = 90) {
  if (!is.list(calibration_points_coord) ||
      length(calibration_points_coord) == 0) {
    stop("`calibration_points_coord` must be a non-empty list.", call. = FALSE)
  }
  .check_vector(degree, "integerish", 1, sign = "positive")
  .check_vector(normalization_angle, "numeric", 1, sign = "positive")
  if (normalization_angle > 180) {
    stop("`normalization_angle` must not exceed 180 degrees.", call. = FALSE)
  }
  for (i in seq_along(calibration_points_coord)) {
    points_name <- paste0("calibration_points_coord[[", i, "]]")
    points <- calibration_points_coord[[i]]
    .check_points_coord(points, points_name)
    if (nrow(points) < 2 || nrow(points) > 19) {
      stop("`", points_name, "` must contain 2 to 19 consecutive pin positions.",
           call. = FALSE)
    }
  }

  series <- lapply(calibration_points_coord, function(points) {
    center <- c(x = points$x[1], y_down = points$y_down[1])
    radius <- sqrt((points$x - center[1])^2 + (points$y_down - center[2])^2)
    list(center = center,
         ds = data.frame(off_axis = (seq_len(nrow(points)) - 1) * 5 * pi / 180,
                         radius = radius))
  })
  ds <- do.call(rbind, lapply(series, function(x) x$ds))
  rownames(ds) <- NULL
  center <- apply(do.call(rbind, lapply(series, function(x) x$center)),
                  2, stats::median)

  fit <- stats::lm(radius ~ poly(off_axis, degree, raw = TRUE) - 1,
                   data = ds)
  coefficients <- unname(stats::coef(fit))
  if (fit$rank < degree || any(!is.finite(coefficients))) {
    stop("Calibration points do not support the requested polynomial degree.",
         call. = FALSE)
  }
  normalization_radius <- rcaiman::calc_relative_radius(
    normalization_angle, coefficients
  )
  projection_coef <- rcaiman::normalize_projection_coef(
    coefficients, normalization_angle
  )
  list(ds = ds,
       model = fit,
       normalization_radius = unname(normalization_radius),
       normalization_angle = normalization_angle,
       projection_coef = projection_coef,
       center_of_vision = center)
}
