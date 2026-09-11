#' Calibrate image geometry
#'
#' @description
#' Calibrate a fisheye lens to derive the mathematical relationship between
#' image-space radial distances from the zenith and zenith angles in
#' hemispherical space (assuming upward-looking hemispherical photography with
#' the optical axis vertically aligned).
#'
#' @details
#' Fisheye lenses have a wide field of view and radial symmetry with respect to
#' distortion. This property allows precise fitting of a polynomial model to
#' relate pixel distances to zenith angles. The method implemented here, known
#' as the "simple method", is described in detail by
#' \insertCite{Diaz2024;textual}{rcaiman.registry}.
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
#' pins at 90º relative to the optical axis, it may be of help to use the naked
#' eye to align the entrance pupil of the lens with the pins. The alignment of
#' the push pins only guarantees the position of the lens entrance pupil, the
#' leveling should be cheeked with an instrument, and the alignment between the
#' optical axis and the radius of the zenith push pin should be taken into
#' account. In practice, the latter is achieved by aligning the camera body with
#' the orthogonal frame made by the quarter circle.
#'
#' Take a photo and transfer it to the computer. Use
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects) to
#' create a point layer. This creates a csv that you can read with
#' [rcaiman::read_points()]. Use the edit tool to add points on the the
#' photographed push pins, starting from the zenith push pin and not skipping
#' any shown push pin. End with an additional point where the image meets the
#' surrounding black (or the last pixel in case there is not blackness because
#' it is not a circular hemispherical image. There is no need to follow the line
#' formed by the push pins).
#' \if{html}{\figure{pushpinsImageJ.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{pushpinsImageJ.jpg}}
#' @note
#' To calibrate different directions, think of the fisheye image as an analog
#' clock. To calibrate 3 o'clock, attach the camera to the tripod in landscape
#' mode while leaving the quarter-circle at the lens's right side. To calibrate
#' 9 o'clock, rotate the camera to put the quarter-circle at the lens's left
#' side. To calibrate 12 and 6 o'clock, do the same but with the camera in
#' portrait mode.
#'
#' @param calibration_points_coord list of `data.frame` objects with numeric
#'   columns `"x"` and `"y_down"`. Each data frame contains points digitized in
#'   the original image frame. The first row identifies the zenith push pin.
#' @param degree numeric vector of length one. Polynomial model degree.
#'
#' @return List with named elements:
#' \describe{
#'   \item{`ds`}{Data frame used to fit the model.}
#'   \item{`model`}{`lm` object fitted to pixel distance vs. zenith angle.}
#'   \item{`horizon_radius`}{Continuous estimate of radial distance in pixels at
#'     90 deg. Review and round this value before supplying it to
#'     [add_geometry_spec()].}
#'   \item{`radial_projection_coef`}{Numeric vector of polynomial model coefficients
#'     for predicting relative radius. Suitable for `parameters` in
#'     [add_geometry_spec()]  with `model = "radial_projection"`.}
#'   \item{`zenith_coord`}{Continuous image coordinates `c(x, y_down)` of the
#'     zenith push pin. Review and snap this estimate to the nearest integer
#'     coordinate before supplying it as `snapped_zenith_coord` to
#'     [add_geometry_spec()].}
#'   \item{`max_theta`}{Maximum zenith angle (deg).}
#'   \item{`max_theta_px`}{Distance in pixels between the zenith and the
#'     maximum zenith angle.}
#' }
#'
#' @references \insertAllCited{}
#'
#' @seealso [validate_projection_function()], [crosscalibrate_geometry()], [calibrate_radiometry()]
#'
#'
#' @export
#'
#' @examples
#' path <- c(system.file("external/a_DSCN7172.csv", package = "rcaiman.registry"),
#'           system.file("external/b_DSCN7162.csv", package = "rcaiman.registry"))
#' calibration_points_coord <- lapply(path, read_points)
#' calibration <- calibrate_geometry(calibration_points_coord)
#' coefficients(calibration$model)
#' calibration$radial_projection_coef %>% signif(3)
#' calibration$horizon_radius
#' calibration$zenith_coord
#'
#' # Constructing a geometry spec remains an explicit human decision.
#' snapped_zenith_coord <- round(calibration$zenith_coord)
#' horizon_radius <- round(calibration$horizon_radius)
#'
#' .fp <- function(theta, radial_projection_coef) {
#'   x <- radial_projection_coef[1:5]
#'   x[is.na(x)] <- 0
#'   for (i in 1:5) assign(letters[i], x[i])
#'   a * theta + b * theta^2 + c * theta^3 + d * theta^4 + e * theta^5
#' }
#'
#' plot(calibration$ds)
#' theta <- seq(0, pi/2, pi/180)
#' lines(theta, .fp(theta, coefficients(calibration$model)))
calibrate_geometry <- function(calibration_points_coord, degree = 3) {
  if (!is.list(calibration_points_coord) ||
      length(calibration_points_coord) == 0) {
    stop("`calibration_points_coord` must be a non-empty list.")
  }
  .check_vector(degree, "integerish", 1, sign = "positive")

  for (i in seq_along(calibration_points_coord)) {
    points_name <- paste0("calibration_points_coord[[", i, "]]")
    .check_points_coord(calibration_points_coord[[i]], points_name)
    if (nrow(calibration_points_coord[[i]]) < 3) {
      stop("`", points_name, "` must contain at least three rows.",
           call. = FALSE)
    }
  }

  .fun <- function(points_coord) {
    coord <- cbind(points_coord$x, points_coord$y_down)
    zenith_coord <- coord[1, ]

    # center in (0,0)
    coord[, 1] <- coord[, 1] - coord[1, 1]
    coord[, 2] <- coord[, 2] - coord[1, 2]

    # Only radial distance is used; the downward y axis does not change it.
    polar <- geometry::cart2pol(coord)
    px <- polar[, 2]
    max_theta_px <- px[length(px)]
    # Remove the data point used only to calculate max_theta_px.
    px <- px[-length(px)]

    theta <- (seq(0, 90, 5)) * pi/180
    theta <- theta[1:length(px)] # because some lens would "see" less pushpins
    ds <- data.frame(theta, px)
    list(ds = ds,
         zenith_coord = zenith_coord,
         max_theta_px = max_theta_px)
  }
  if (length(calibration_points_coord) > 1) {
    l <- lapply(calibration_points_coord, .fun)
    ds <- l[[1]]$ds
    for (i in 2:length(calibration_points_coord)) {
      ds <- rbind(ds, l[[i]]$ds)
    }
    zenith_coord <- Map(function(x) x$zenith_coord, l) %>% unlist() %>%
      matrix(., ncol = 2, byrow = TRUE) %>% apply(., 2, stats::median)
    max_theta_px <- Map(function(x) x$max_theta_px, l) %>% unlist() %>%
      stats::median()
    l <- list(ds = ds,
              zenith_coord = zenith_coord,
              max_theta_px = max_theta_px)
  } else {
    l <- .fun(calibration_points_coord[[1]])
  }
  theta <- l$ds$theta
  px <- l$ds$px

  fit <- lm(theta ~ poly(px, degree, raw = TRUE) - 1)
  lens_coef_caneye <- unname(coefficients(fit))
  max_theta <- stats::predict(fit, data.frame(px = l$max_theta_px)) * 180/pi

  fit <- lm(px ~ poly(theta, degree, raw = TRUE) - 1)
  horizon_radius <- stats::predict(fit, data.frame(theta = pi / 2)) %>% unname()

  list(ds = l$ds,
       model = fit,
       horizon_radius = horizon_radius,
       radial_projection_coef =
         (coefficients(fit) / horizon_radius) %>% unname(),
       zenith_coord = setNames(l$zenith_coord, c("x", "y_down")),
       max_theta = max_theta %>% unname(),
       max_theta_px = l$max_theta_px)
}
