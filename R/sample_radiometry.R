#' Sample radiometry around internal viewing directions
#'
#' @description
#' Extract pixels in angular neighborhoods and summarize their digital numbers
#' for radiometric calibration.
#'
#' @details
#' Prepare a single-band image before sampling. For the simple method, this
#' can be the cell-wise maximum of a sequence acquired with unchanged exposure
#' and lighting. The image, internal geometry, and cell indices must share the
#' original band grid. No crop, expansion, or reprojection is performed.
#'
#' Query directions define the angular centers of the measurements, in the
#' internal reference frame. They are used as supplied, without mapping them
#' to cells. For digitized queries, explicitly convert coordinates to indices
#' and then use [rcaiman::add_directions()] in the script. Alternatively,
#' [rcaiman::fibonacci_directions()] generates internal angular queries
#' independently of the image grid. For cross-calibration,
#' supply the same query directions to both images, under the alignment
#' assumptions described in [crosscalibrate_radiometry()].
#'
#' Sampling indices identify eligible
#' pixels, for example all cells admitted by a manually digitized mask in
#' rcaiman-projects. [rcaiman::add_directions()] supplies their internal angles
#' and [rcaiman::nn_spherical()] selects every candidate within the angular
#' radius of each query. External orientation is not needed.
#'
#' The measurement is the arithmetic mean of the selected pixel values, without
#' distance or solid-angle weighting. A pixel may contribute to more than one
#' query. Its row is then repeated in `points`, with the corresponding query
#' identifier. Repeated input indices are retained as repeated observations.
#' A query without neighbors has `n_samples = 0` and `dn = NA`.
#'
#' Digital numbers are returned without normalization. Inspect the selected
#' pixels before passing `measurements` to [calibrate_radiometry()] or paired
#' measurement tables to [crosscalibrate_radiometry()]. Use
#' [rcaiman::rem_unselected()] beforehand with a mask of eligible pixels.
#' Combine it with `!is.na(r)`, `!is.na(internal_geometry$off_axis)`, and
#' `!is.na(internal_geometry$internal_azimuth)` to exclude missing image
#' values or internal directions.
#'
#' @param r numeric [terra::SpatRaster-class] with one band to sample.
#' @param internal_geometry [terra::SpatRaster-class] from
#'   [rcaiman::set_internal_geometry()], on the same grid as `r`.
#' @param query_directions data frame with finite numeric `off_axis` in
#'   `[0, pi]` and `internal_azimuth` in `[0, 2 * pi)`, both in radians.
#'   Additional columns are preserved in `measurements`. Any `col` and `row`
#'   columns are metadata and are not used to locate or select pixels.
#' @param sampling_indices data frame with positive
#'   integer-like `col` and `row` columns. Convert digitized coordinates with
#'   [rcaiman::indices_from_coord()]. Additional columns are preserved.
#' @param search_radius positive numeric vector of length one, at most 180.
#'   Neighborhood radius in degrees. Defaults to 2.
#' @param group character vector of length one identifying this acquisition
#'   group. Defaults to `"1"`. Use distinct groups for series that will be
#'   normalized separately, for example `"3"`, `"9"`, `"12"`, and `"6"`.
#'   [crosscalibrate_radiometry()] requires a single group in each table,
#'   since it compares one image pair per call.
#'
#' @return list with two data frames:
#'   \describe{
#'     \item{`points`}{Selected rows of `sampling_indices`, with `group`,
#'       `sample_id` (input sampling row), `query_id` (input query row),
#'       `off_axis`, `internal_azimuth`, angular `distance`, and `dn`.
#'       All angles are in radians. Rows are grouped by query and ordered by
#'       increasing distance. Cell indices refer to the unchanged input grid.}
#'     \item{`measurements`}{All rows and columns of `query_directions`, in
#'       input order, with `group`, `query_id`, `n_samples`, and mean `dn`.
#'       The query angles are preserved, not replaced by candidate directions.}
#'   }
#'   Names of added columns must not already exist in the respective input.
#'
#' @section Guidance:
#' Light attenuation generally increases with off-axis angle. This vignetting
#' effect varies with lens model and aperture setting. The method
#' outlined here, known as the simple method, is explained in detail in
#' \insertCite{Diaz2024;textual}{rcaiman.registry}. The next explanation might
#' serve mostly as a quick recap of it.
#'
#' The development of the simple method was done with a Kindle Paperwhite eBook
#' reader of 6" with built-in light. However, an iPhone 6 Plus was also tested
#' in the early stages of development and no substantial differences were
#' observed. A metal bookend was used to fasten the eBook reader upright and a
#' semi-transparent paper to favor a Lambertian light distribution. In addition,
#' the latter was used to draw on, in order to guide pixel sampling. The bookend
#' also facilitated the alignment of the screen with the dotted lines of the
#' printed quarter-circle.
#' \if{html}{\figure{lightSource.jpg}{options: style="display:block;margin:0 auto;max-width:50%;"}}
#' \if{latex}{\figure{lightSource.jpg}}
#' As a general guideline, a wide variety of mobile devices could be used as
#' light sources, but if scattered data points are obtained with it, then other
#' light sources should be tested in order to double-check that the light
#' quality is not the reason for points scattering.
#'
#' With the room only illuminated by the portable light source, nine photographs
#' should be taken with the light source located in the equivalent to 0, 10, 20,
#' 30, 40, 50, 60, 70, and 80 degrees of off-axis angle, respectively. Camera
#' configuration should be in manual mode and set with the aperture (f/number)
#' for which a vignetting function is required. The shutter speed should be
#' regulated to obtain light-source pixels with middle grey values. The nine
#' photographs should be taken **without changing the camera configuration and
#' the light conditions**.
#' \if{html}{\figure{calibrationBoardVignetting.jpg}{options: style="display:block;margin:0 auto;max-width:50%;"}}
#' \if{latex}{\figure{calibrationBoardVignetting.jpg}}
#'
#' In the current recommended workflow,
#' [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects) can
#' be used via the templates found in its menu *Help>Examples*.
#'
#' @note
#' To calibrate different directions, think of the fisheye image as an analog
#' clock. To calibrate 3 o'clock, attach the camera to the tripod in landscape
#' mode while leaving the quarter-circle at the lens's right side. To calibrate
#' 9 o'clock, rotate the camera to put the quarter-circle at the lens's left
#' side. To calibrate 12 and 6 o'clock, do the same but with the camera in
#' portrait mode.
#'
#' @references \insertAllCited{}
#' @seealso [calibrate_radiometry()], [rcaiman::add_directions()],
#'   [rcaiman::nn_spherical()]
#' @export
#' @examples
#' # An image and its internal geometry on the same rectangular band grid.
#' geometry <- list(
#'   model = "radial_projection",
#'   parameters = equidistant_coef,
#'   center_of_vision = c(20, 15),
#'   normalization_radius = 30,
#'   normalization_angle = 90
#' )
#' ig <- set_internal_geometry(c(40, 30), geometry)
#'
#' # Synthetic digital numbers, for an executable example.
#' r <- 1000 * (1 - 0.15 * (ig[["off_axis"]] * pi / 180)^2)
#' query_coord <- data.frame(x = c(19.5, 22.5, 25.5, 28.5, 31.5),
#'                           y_down = 14.5)
#' query_indices <- indices_from_coord(query_coord$x, query_coord$y_down)
#' query_directions <- add_directions(query_indices, ig)
#'
#' # In a project, these candidates would come from the manually chosen mask.
#' sampling_indices <- expand.grid(col = 1:40, row = 1:30)
#' m <- !is.na(r) & !is.na(ig$off_axis) & !is.na(ig$internal_azimuth)
#' sampling_indices <- rem_unselected(sampling_indices, m)
#' samples <- sample_radiometry(r, ig, query_directions, sampling_indices,
#'                              search_radius = 4, group = "3")
#' plot(r)
#' pixels <- samples$points
#' xy <- xy_from_indices(pixels$col, pixels$row, nrow(r))
#' graphics::points(xy, col = pixels$query_id, pch = 20)
#'
#' # Export selected pixel centers for inspection as a point layer.
#' sample_coord <- cbind(coord_from_indices(pixels$col, pixels$row), pixels)
#' write_points(sample_coord, file.path(tempdir(), "radiometry_samples.csv"))
#' samples$measurements
#' calibration <- calibrate_radiometry(samples$measurements, degree = 2)
#'
#' # Angular queries can also be generated directly, without digitization.
#' query_directions <- fibonacci_directions(10, max_off_axis_angle = 60)
#' samples <- sample_radiometry(r, ig, query_directions, sampling_indices)
#'
#' # Reuse query_directions for another image when cross-calibrating.
sample_radiometry <- function(r, internal_geometry, query_directions,
                               sampling_indices, search_radius = 2,
                               group = "1") {
  .assert_single_layer(r)
  .assert_spatraster(internal_geometry)
  .assert_same_geom(r, internal_geometry)
  .check_vector(group, "character", 1)
  if (!nzchar(group)) stop("`group` must not be empty.", call. = FALSE)
  .check_columns(query_directions, c("off_axis", "internal_azimuth"),
                  c("group", "query_id", "n_samples", "dn"))
  .check_vector(query_directions$off_axis, "numeric", name = "query_directions$off_axis")
  .check_vector(query_directions$internal_azimuth, "numeric",
                name = "query_directions$internal_azimuth")
  if (any(query_directions$off_axis < 0 | query_directions$off_axis > pi) ||
      any(query_directions$internal_azimuth < 0 |
          query_directions$internal_azimuth >= 2 * pi)) {
    stop("Query angles must be in [0, pi] and [0, 2 * pi) radians.", call. = FALSE)
  }
  .check_columns(sampling_indices, c("col", "row"),
                  c("group", "sample_id", "query_id", "distance", "dn"))
  queries <- query_directions
  candidates <- add_directions(sampling_indices, internal_geometry)
  # Rename only the extraction copy, so the image's band name is preserved.
  band <- r[[1]]
  names(band) <- "dn"
  candidates <- extract_dn(band, candidates, use_window = FALSE)
  .check_vector(candidates$dn, "numeric", name = "sampled digital numbers")
  neighbors <- nn_spherical(
    candidates, queries, k = NULL, search_radius = search_radius,
    angle_columns = c("off_axis", "internal_azimuth"), parallel = FALSE
  )
  n_samples <- lengths(lapply(neighbors, function(x) x$index))
  sample_id <- as.integer(unlist(lapply(neighbors, function(x) x$index),
                                 use.names = FALSE))
  points <- candidates[sample_id, , drop = FALSE]
  points$group <- rep(group, nrow(points))
  points$sample_id <- sample_id
  points$query_id <- rep(seq_len(nrow(queries)), n_samples)
  points$distance <- as.numeric(unlist(lapply(neighbors, function(x) x$distance),
                                      use.names = FALSE))
  rownames(points) <- NULL
  queries$group <- rep(group, nrow(queries))
  queries$query_id <- seq_len(nrow(queries))
  queries$n_samples <- n_samples
  queries$dn <- vapply(neighbors, function(x) {
    if (!length(x$index)) return(NA_real_)
    mean(candidates$dn[x$index])
  }, numeric(1))
  list(points = points, measurements = queries)
}
