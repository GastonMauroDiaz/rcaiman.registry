#' Calculate zenith raster coordinates
#'
#' @description
#' Calculate zenith raster coordinates from image coordinates
#'
#' @details
#' In this context, “zenith” denotes the location in the image that corresponds
#' to the projection of the vertical direction when the optical axis is aligned
#' vertically.
#'
#' The technique described under the headline ‘Optical center characterization’
#' of the [user manual of the software
#' Can-Eye](https://can-eye.paca.hub.inrae.fr/documentation/documentation) can
#' be used to acquire the data for determining the zenith coordinates. This
#' technique was used by \insertCite{Pekin2009;textual}{rcaiman}, among others.
#' Briefly, it consists in drilling a small hole in the cap of the fisheye lens
#' (away from the center), and taking about ten photographs without removing the
#' cap. The cap must be rotated about 30º before taking each photograph.
#'
#' Another method (only valid when enough of the circle perimeter is depicted in
#' the image) is taking a very bright picture (e.g., of a white-painted corner
#' of a room) with the lens uncovered (do not use any mount). Then, digitize
#' points over the circle perimeter. This was the method used for producing the
#' example file (see Examples). It is worth noting that the perimeter of the
#' circle depicted in a circular hemispherical photograph is not necessarily the
#' horizon.
#'
#' The recomended tool to obtain the points is
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects)
#'
#' @note
#' This function assumes that all data points belong to the same circle, meaning
#' that it does not support multiple holes when the Can-Eye procedure of
#' drilling the lens cap is applied. The circle is fitted using the method
#' presented by \insertCite{Kasa1976;textual}{rcaiman.registry}.
#'
#' Coordinates follow image (raster) convention: the origin is in the
#' upper-left, and the vertical axis increases downward, like a spreadsheet.
#' This contrasts with Cartesian coordinates, where the vertical axis increases
#' upward.
#'
#' @param pts `data.frame` with at least columns `"row"` and `"col"`,
#'   giving raster coordinates of the digitized points.
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @return Named numeric vector of length two. Raster coordinates of the zenith.
#'
#'
#' @examples
#' path <- system.file("external/DSCN6089.csv",
#'                     package = "rcaiman")
#' calc_zenith_col_row(read_points(path))
calc_zenith_col_row <- function(pts) {
  x <- pts$col
  y <- pts$row
  A <- cbind(2 * x, 2 * y, rep(1, length(x)))
  b <- x^2 + y^2
  sol <- solve(t(A) %*% A, t(A) %*% b)

  cx <- sol[1]
  cy <- sol[2]

  out <- c(col = cx, row = cy)
  out
}
