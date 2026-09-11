#' Calculate zenith image coordinates
#'
#' @description
#' Estimate the continuous image coordinates of the zenith by fitting a circle
#' to points digitized along a circular image feature.
#'
#' @details
#' In this context, "zenith" denotes the location in the image that corresponds
#' to the projection of the vertical direction when the optical axis is aligned
#' vertically.
#'
#' The technique described under the headline "Optical center characterization"
#' of the [Can-Eye user
#' manual](https://can-eye.paca.hub.inrae.fr/documentation/documentation) can be
#' used to acquire the data for determining the zenith coordinates. This
#' technique was used by \insertCite{Pekin2009;textual}{rcaiman.registry}, among
#' others. Briefly, it consists in drilling a small hole in the cap of the
#' fisheye lens away from its center and taking about ten photographs without
#' removing the cap. Rotate the cap about 30 deg before taking each photograph.
#'
#' Another method, valid only when enough of the circle perimeter is depicted in
#' the image, is to take a bright photograph with the lens uncovered and then
#' digitize points along the circle perimeter. The perimeter of a circular
#' hemispherical photograph does not necessarily represent the horizon.
#'
#' Use
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects) to
#' create and edit the point layer in the original image frame.
#'
#' The circle is fitted with the method presented by
#' \insertCite{Kasa1976;textual}{rcaiman.registry}.
#'
#' @param points_coord `data.frame` with numeric columns `"x"` and `"y_down"`.
#'   Points digitized along the same circular feature in the original image
#'   frame, using the continuous image coordinate convention of
#'   [rcaiman::y_down_from_y()].
#'
#' @return Named numeric vector of length two, `c(x, y_down)`, containing the
#'   continuous image coordinates of the estimated zenith.
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#' path <- system.file("external/DSCN6089.csv", package = "rcaiman.registry")
#' calc_zenith_coord(rcaiman::read_points(path))
calc_zenith_coord <- function(points_coord) {
  .check_points_coord(points_coord)
  if (nrow(points_coord) < 3) {
    stop("`points_coord` must contain at least three rows.", call. = FALSE)
  }

  x <- points_coord$x
  y_down <- points_coord$y_down
  A <- cbind(2 * x, 2 * y_down, rep(1, length(x)))
  b <- x^2 + y_down^2
  sol <- solve(t(A) %*% A, t(A) %*% b)

  c(x = unname(sol[1]), y_down = unname(sol[2]))
}
