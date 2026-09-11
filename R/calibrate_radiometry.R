.check_calibration_data <- function(calibration_data) {
  if (!is.list(calibration_data) || length(calibration_data) == 0) {
    stop("`calibration_data` must be a non-empty named list.", call. = FALSE)
  }
  if (is.null(names(calibration_data))
      || any(is.na(names(calibration_data)))
      || any(!nzchar(names(calibration_data)))) {
    stop("`calibration_data` must be a named list.", call. = FALSE)
  }

  required <- c("images", "query_points", "sampling_points")
  for (i in seq_along(calibration_data)) {
    x <- calibration_data[[i]]
    group <- names(calibration_data)[i]
    if (!is.list(x)) {
      stop("Calibration case `", group, "` must be a list.", call. = FALSE)
    }
    missing <- setdiff(required, names(x))
    if (length(missing) > 0) {
      stop(
        "Calibration case `", group, "` is missing: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    .check_vector(x$images, "character", 1)
    .check_vector(x$query_points, "character", 1)
    .check_vector(x$sampling_points, "character", 1)
    if (!dir.exists(x$images)) {
      stop("Image folder does not exist for calibration case `", group, "`.",
           call. = FALSE)
    }
    .assert_file_exists(c(x$query_points, x$sampling_points))
  }
  invisible(TRUE)
}

.prepare_radiometry_image <- function(caim,
                                      z,
                                      snapped_zenith_coord,
                                      horizon_radius,
                                      is_horizon_circle_clipped) {
  if (is_horizon_circle_clipped) {
    expand_non_circular(caim, z, snapped_zenith_coord)
  } else {
    crop_caim(
      caim,
      snapped_zenith_coord = snapped_zenith_coord,
      horizon_radius = horizon_radius
    )
  }
}

.prepare_radiometry_indices <- function(sampling_coord,
                                        z,
                                        snapped_zenith_coord,
                                        horizon_radius,
                                        is_horizon_circle_clipped) {
  if (is_horizon_circle_clipped) {
    sampling_coord <- expand_non_circular_points(
      sampling_coord,
      z,
      snapped_zenith_coord
    )
  } else {
    sampling_coord <- crop_points(
      sampling_coord,
      snapped_zenith_coord = snapped_zenith_coord,
      horizon_radius = horizon_radius
    )
  }

  sampling_indices <- indices_from_coord(
    sampling_coord$x,
    sampling_coord$y_down
  )
  nonspatial_columns <- setdiff(
    names(sampling_coord),
    c("x", "y_down", "col", "row")
  )
  sampling_indices[nonspatial_columns] <- sampling_coord[nonspatial_columns]
  rownames(sampling_indices) <- rownames(sampling_coord)
  sampling_indices
}

.extract_radiometry_case <- function(x,
                                     group,
                                     z,
                                     a,
                                     snapped_zenith_coord,
                                     horizon_radius,
                                     band,
                                     search_radius,
                                     is_horizon_circle_clipped) {
  query_coord <- read_points(x$query_points)
  query_indices <- .prepare_radiometry_indices(
    query_coord,
    z,
    snapped_zenith_coord,
    horizon_radius,
    is_horizon_circle_clipped
  )

  i <- extract_dn(
    z,
    query_indices[, c("col", "row")],
    use_window = FALSE
  )[, 3]
  i <- is.na(i)
  if (any(i))
    stop(
      "Please revise the query points of direction ",
      group,
      ". ",
      sum(i),
      " points fall outside the ",
      if (is_horizon_circle_clipped) {
        "circular image."
      } else {
        "expanded circular image."
      }
    )
  query_indices <- add_directions(query_indices, z, a)

  sampling_coord <- read_points(x$sampling_points)
  sampling_indices <- .prepare_radiometry_indices(
    sampling_coord,
    z,
    snapped_zenith_coord,
    horizon_radius,
    is_horizon_circle_clipped
  )

  i <- extract_dn(
    z,
    sampling_indices[, c("col", "row")],
    use_window = FALSE
  )[, 3]
  i <- !is.na(i)
  sampling_indices <- add_directions(sampling_indices[i, ], z, a)

  nn <- nn_spherical(
    sampling_indices,
    query_indices,
    k = nrow(sampling_indices),
    search_radius = search_radius
  )

  files <- dir(
    x$images,
    pattern = "\\.tif(f)?$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) {
    stop("No TIFF images found in `", x$images, "`.", call. = FALSE)
  }

  rasters <- lapply(files, function(path_to_file) {
    caim <- read_caim(path_to_file)
    if (!band %in% names(caim)) {
      stop(
        "Band `", band, "` not found in image `", path_to_file, "`.",
        call. = FALSE
      )
    }
    .prepare_radiometry_image(
      caim[[band]],
      z,
      snapped_zenith_coord,
      horizon_radius,
      is_horizon_circle_clipped
    )
  })

  r <- terra::rast(rasters)
  r <- max(r)

  dn <- numeric(length(nn))
  for (i in seq_along(nn)) {
    sampling_subset <- sampling_indices[
      nn[[i]]$index,
      c("col", "row")
    ]
    dn[i] <- mean(
      extract_dn(r, sampling_subset, use_window = FALSE)[, 3]
    )
  }

  ds <- data.frame(theta = query_indices$zen, radiometry = dn)
  ds <- rbind(ds, transform(ds, theta = -theta))
  ds$radiometry <- abs(ds$radiometry)
  spline_fun <- stats::splinefun(ds)
  ds <- ds[seq_len(nrow(ds) / 2), ]
  ds$radiometry <- ds$radiometry / spline_fun(0)
  ds$group <- group
  ds <- ds[, c("group", "theta", "radiometry")]
  ds
}

#' Calibrate radiometry
#'
#' @description
#' Calibrate a fisheye lens to derive the mathematical relationship between
#' zenith angle and inside-camera relative radiance when the photographed target
#' is of constant radiance.
#'
#' @section Guidance:
#' Lenses have the inconvenient property of increasingly attenuating light along
#' the direction orthogonal to the optical axis. This phenomenon is known as the
#' vignetting effect and varies with lens model and aperture setting. The method
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
#' 30, 40, 50, 60, 70, and 80 degrees of zenith angle, respectively. Camera
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
#' @param calibration_data named list of calibration cases. Each element
#'   represents one acquisition group and must contain `images`, `query_points`,
#'   and `sampling_points`. (These inputs can be prepared conveniently with
#'   [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects),
#'   but they are plain files consumed through rcaiman.)
#'   \describe{
#'     \item{`images`}{Path to a folder containing images readable by
#'     [rcaiman::read_caim()], typically CFA-split
#'     images produced with [rcaiman::split_cfa()] and interpreted with
#'     [rcaiman::map_spectral_bands()].}
#'     \item{`query_points`}{Path to a point CSV readable by
#'     [rcaiman::read_points()] with the position manually entered on the imaged
#'     central crosses (the cross drawn on the paper covering the eBook screen,
#'     see *Guidance*) as depicted in `images`.}
#'     \item{`sampling_points`}{Path to a point CSV readable by
#'     [rcaiman::read_points()] and used to estimate radiometry around each
#'     query point. The feature "Create grid..." of *rcaiman-projects* is very
#'     convenient to obtain these points.}
#'   }
#'   The names of `calibration_data` are preserved in the returned dataset as a
#'   grouping variable, and are intended to identify the sampled direction, for
#'   example `"12"`, `"3"`, `"6"` and `"9"` (see *Note*).
#' @param snapped_zenith_coord integer-like numeric vector of length two.
#'   Continuous image coordinates of the snapped optical center in the original
#'   image frame, in the order `c(x, y_down)`. The coordinates identify the
#'   center of the nearest 2 x 2 cell window.
#' @param horizon_radius positive integer-like numeric vector of length one.
#'   Radius of the processing geometry in cell units. Twice this value is the
#'   diameter supplied to [rcaiman::zenith_image()].
#' @param projection_coef numeric vector. Polynomial coefficients defining the
#'   radial projection model evaluated in radians.
#' @param is_horizon_circle_clipped logical vector of length one. Indicates
#'   whether the horizon circle (90 deg zenith angle), obtained via model
#'   extrapolation when necessary, is clipped by the image matrix. If
#'   `FALSE`, the geometry specification asserts that the complete horizon
#'   circle is fully
#'   included within the image extent.
#' @param degree integer-like vector of length one. Polynomial model degree.
#' @param band character vector of length one. Name of the band to sample from
#'   each image on argument `calibration_data`.
#' @param search_radius numeric vector of length one. Search radius in radians
#'   used by `rcaiman::nn_spherical()` to associate sampling points with each
#'   query point.
#'
#' @note
#' Point CSVs are persistent annotations in the original image frame. For a
#' clipped horizon, images and points are transformed with
#' [rcaiman::expand_non_circular()] and
#' [rcaiman::expand_non_circular_points()]. Otherwise, both are cropped using
#' `snapped_zenith_coord` and `horizon_radius`. Continuous coordinates are
#' assigned to cells with [rcaiman::indices_from_coord()] only after this common
#' preprocessing, immediately before digital-number and direction extraction.
#'
#' The function does not verify that coordinates, image dimensions, and the
#' supplied geometry describe the same acquisition frame.
#'
#' To calibrate different directions, think of the fisheye image as an analog
#' clock. To calibrate 3 o'clock, attach the camera to the tripod in landscape
#' mode while leaving the quarter-circle at the lens's right side. To calibrate
#' 9 o'clock, rotate the camera to put the quarter-circle at the lens's left
#' side. To calibrate 12 and 6 o'clock, do the same but with the camera in
#' portrait mode.
#'
#' @return List with named elements:
#' \describe{
#'   \item{`ds`}{data frame used to fit the model, with `group`, `theta` in
#'     radians, and normalized `radiometry`.}
#'   \item{`model`}{`lm` object fitted to `1 - radiometry` as a polynomial
#'     function of `theta`, without an intercept.}
#'   \item{`vignetting_coef`}{numeric vector of polynomial coefficients derived
#'     from `model`, suitable for `model = "ff_simple_polynomial"` in
#'     [add_radiometry_spec()].}
#' }
#'
#' @references \insertAllCited{}
#'
#' @seealso [calibrate_geometry()], [add_radiometry_spec()]
#'
#' @export
calibrate_radiometry <- function(calibration_data,
                                 is_horizon_circle_clipped,
                                 snapped_zenith_coord,
                                 horizon_radius,
                                 projection_coef,
                                 degree = 3,
                                 band = "Blue",
                                 search_radius = 2) {

  .check_calibration_data(calibration_data)
  .check_vector(is_horizon_circle_clipped, "logical", 1)
  .check_vector(
    snapped_zenith_coord,
    "integerish",
    2,
    sign = "positive"
  )
  .check_vector(horizon_radius, "integerish", 1, sign = "positive")
  .check_vector(projection_coef, "numeric", sign = "any")
  .check_vector(degree, "integerish", 1, sign = "positive")
  .check_vector(band, "character", 1)
  .check_vector(search_radius, "numeric", 1, sign = "positive")

  side <- horizon_radius * 2

  z <- zenith_image(side, projection_coef)
  a <- azimuth_image(z)

  ds <- Map(
    function(x, group) {
      .extract_radiometry_case(
        x,
        group = group,
        z = z,
        a = a,
        snapped_zenith_coord = snapped_zenith_coord,
        horizon_radius = horizon_radius,
        band = band,
        search_radius = search_radius,
        is_horizon_circle_clipped = is_horizon_circle_clipped
      )
    },
    calibration_data,
    names(calibration_data)
  )
  ds <- do.call(rbind, ds)
  rownames(ds) <- NULL

  fit <- stats::lm(
    (1 - radiometry) ~ stats::poly(theta, degree, raw = TRUE) - 1,
    data = ds
  )
  vignetting_coef <- -stats::coefficients(fit)

  list(
    ds = ds,
    model = fit,
    vignetting_coef = unname(vignetting_coef)
  )
}
