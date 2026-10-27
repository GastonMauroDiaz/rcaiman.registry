#' Estimate the center of vision
#'
#' @description
#' Estimate the continuous image coordinates of the center of vision by
#' fitting a circle to digitized points.
#'
#' @details
#' The circle is fitted with the algebraic method presented by
#' \insertCite{Kasa1976;textual}{rcaiman.registry}. Its center is used as an
#' estimate of the center of vision. All supplied points contribute equally
#' to the fit. The acquisition procedures below provide the circular feature
#' to be digitized in the original image frame.
#'
#' ### Acquiring the points
#'
#' The technique described under "Optical center characterization" in the
#' [Can-Eye user manual](https://can-eye.paca.hub.inrae.fr/documentation/documentation)
#' can be used to acquire the data. It was used by
#' \insertCite{Pekin2009;textual}{rcaiman.registry}, among others. Drill a small
#' hole in the fisheye lens cap away from its center and take about ten
#' photographs without removing the cap. Rotate the cap about 30 deg before
#' taking each photograph, and digitize the successive positions of the spot.
#'
#' Another method, when enough of the image circle perimeter is visible, is
#' to take a bright photograph with the lens uncovered and digitize points
#' along that perimeter. A complete circle need not be visible.
#'
#' Use [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects)
#' to create and edit the point layer in the original image frame.
#'
#' ### Choosing the definitive center
#'
#' The returned estimate is continuous. Use `round()` explicitly to choose
#' the integer-coordinate center required by [add_geometry_spec()] and
#' [rcaiman::set_internal_geometry()]. Integer coordinates lie on pixel
#' boundaries. For [crosscalibrate_geometry()], choose that definitive center
#' before fitting and retain it in the resulting geometry specification.
#'
#' ## Alternative
#'
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects)
#' offers a measuring tool in View>Measure triangle that can be used to obtain
#' the center of vision on the fly, i.e., there is no trazability to that operation.
#'
#' @param points_coord data frame with finite numeric columns `x` and
#'   `y_down`, containing at least three non-collinear points on the same
#'   circular feature. Coordinates follow [rcaiman::y_down_from_y()]: the
#'   origin is the upper-left image corner, increasing rightward and downward.
#'   Additional columns are ignored.
#'
#' @return Named numeric vector of length two, `c(x, y_down)`, with the
#'   estimated center of vision in continuous image coordinates. The estimate
#'   is not rounded.
#'
#' @references \insertAllCited{}
#' @seealso [crosscalibrate_geometry()], [add_geometry_spec()]
#' @export
#' @examples
#' path <- system.file("external/DSCN6089.csv", package = "rcaiman.registry")
#' points_coord <- read_points(path)
#' calc_center_of_vision(points_coord)
calc_center_of_vision <- function(points_coord) {
  .check_points_coord(points_coord)
  if (nrow(points_coord) < 3) {
    stop("`points_coord` must contain at least three non-collinear points.",
         call. = FALSE)
  }

  origin <- c(mean(points_coord$x), mean(points_coord$y_down))
  x <- points_coord$x - origin[1]
  y_down <- points_coord$y_down - origin[2]
  # Translation preserves the algebraic circle fit; QR avoids normal equations.
  A <- cbind(2 * x, 2 * y_down, 1)
  fit <- qr(A)
  if (fit$rank < 3) {
    stop("`points_coord` must contain at least three non-collinear points.",
         call. = FALSE)
  }
  sol <- qr.coef(fit, x^2 + y_down^2)
  center <- origin + unname(sol[1:2])
  c(x = center[1], y_down = center[2])
}
