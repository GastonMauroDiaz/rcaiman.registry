#' Cross-calibrate radiometry from paired measurements
#'
#' @description
#' Estimate a relative-response model by comparing measurements from an
#' uncalibrated system with those from a radiometrically corrected reference
#' image.
#'
#' @section Acquisition and sampling:
#'
#' The user is responsible for acquiring images whose corresponding internal
#' directions represent the same external directions: align the optical axes
#' and orient the tops of both sensors identically. Use stable illumination
#' and linear digital numbers from the bands selected for comparison. The
#' function assumes these conditions and does not detect, estimate, or
#' compensate for orientation differences or experimental errors.
#'
#' The reference measurements must be free of radiometric bias.
#' [sample_radiometry()] selects and returns pixels and their mean digital
#' numbers independently for each image. Supply the same `query_directions`
#' table to both calls and choose eligible pixels in each image. Shared queries
#' fix the angular centers, while the contributing cells depend on each image's
#' geometry and mask.
#'
#' Images grids and pixel indices may differ.
#' Differences in geometric calibration errors between the two systems can
#' cause them to sample different external directions. The resulting radiance
#' differences propagate into the radiometric fit, particularly across steep
#'
#' Use masks to exclude abrupt radiance gradients. Direct sunlight must not
#' enter the lens.
#'
#' Each call compares one pair of images. Supply their measurement tables in
#' corresponding row order. Filtering or reordering must preserve the pairs.
#' Rows are not joined by angle or identifier, and missing or nonpositive
#' measurements are not removed automatically. If `group` is present, each
#' table must contain exactly one nonmissing, nonempty group. Its name may
#' differ between tables; it is not used to match rows. Additional columns
#' are preserved in the returned profiles.
#'
#' @details
#' From the paired measurements prepared as described in *Acquisition and
#' sampling*, the function estimates an optical-axis reference for each system,
#' normalizes the measurements, and fits a relative-response model.
#'
#' For each system, the function selects the `k` measurements nearest to the
#' optical axis. Their angular distances are their own `off_axis` values.
#' It estimates the digital number on the axis by inverse-square distance
#' weighting:
#'
#' \deqn{\widehat{DN}_0 = \frac{\sum_i DN_i/d_i^2}{\sum_i 1/d_i^2}.}
#'
#' This is the estimator used for the zenith reference in
#' [rcaiman::calc_relative_radiance()], applied here to the optical axis.
#' No pixel has to coincide with the axis. `k` defaults to 3; distance ties
#' are resolved in input order. If selected measurements have zero distance,
#' only those contribute, with equal weights. `reference_weight` in each
#' returned profile records the normalized weights; other rows have weight zero.
#'
#' The function normalizes each table by its own reference and divides the
#' corresponding normalized measurements:
#'
#' \deqn{f_{v,i} =
#' \frac{DN_{uncal,i}/\widehat{DN}_{uncal,0}}
#'      {DN_{cal,i}/\widehat{DN}_{cal,0}}.}
#'
#' The result is `relative_response`. It is a ratio of normalized neighborhood
#' means, not a mean of pixel-wise ratios. Multiplicative exposure differences
#' cancel through the separate normalizations. The estimator is local: its
#' accuracy depends on the measurements near the optical axis.
#'
#' It then fits `1 - relative_response` as a polynomial of the uncalibrated
#' system's `off_axis` in radians, without an intercept. All measurements enter with
#' equal weight. The resulting simple polynomial fixes the response at the
#' axis to one. Models and their evaluation are described in
#' [rcaiman::calc_relative_response()]. The normalization is not repeated
#' during fitting; [calibrate_radiometry()] uses its own normalization for the
#' simple method and shares only the polynomial fitting operation.
#'
#' @param measurements_uncal,measurements_cal data frames, normally the
#'   `measurements` components from [sample_radiometry()]. Required columns
#'   are finite numeric `off_axis` in radians (`[0, pi]`) and strictly positive
#'   finite `dn`. Rows correspond by position and the tables must have equal
#'   lengths. Additional columns are retained in the returned profiles;
#'   `group`, when present, must identify a single acquisition in each table.
#'   `reference_dn`, `normalized_dn`, and `reference_weight` are reserved.
#' @param degree positive integer-like numeric vector of length one.
#'   Polynomial degree, from 1 to 6, default 5. The observations must support
#'   estimation of all coefficients.
#' @param k positive integer-like numeric vector of length one. Number of
#'   measurements used to estimate each optical-axis reference, default 3.
#'   Each table must contain at least `k` rows.
#'
#' @return List containing:
#' \describe{
#'   \item{`ds`}{Paired data in input order, with `off_axis` for the uncalibrated
#'     system, `off_axis_cal`, `dn_uncal`, `dn_cal`, `normalized_dn_uncal`,
#'     `normalized_dn_cal`, and `relative_response`. Angles are in radians.}
#'   \item{`model`}{Fitted [stats::lm] predicting `1 - relative_response` from
#'     `off_axis` in radians. Residuals combine experimental and model errors.}
#'   \item{`vignetting_coef`}{Numeric coefficients without an intercept, in
#'     increasing order of degree, for `model = "simple_polynomial"` in
#'     [add_radiometry_spec()] or [rcaiman::flat_field_image()].}
#'   \item{`profile_uncal`, `profile_cal`}{Original tables, in corresponding
#'     row order, with `reference_dn`, `normalized_dn`, and `reference_weight`
#'     added.}
#'   \item{`reference_dn`}{Named numeric vector with the estimated optical-axis
#'     references, `uncal` and `cal`.}
#' }
#'
#' @seealso [sample_radiometry()], [rcaiman::calc_relative_response()]
#' @export
#' @examples
#' \dontrun{
#' # Prepared images on their original grids; r_cal is radiometrically corrected.
#' # query_directions contains off_axis and internal_azimuth in radians.
#' # Use the same angular queries for both images.
#' samples_uncal <- sample_radiometry(
#'   r_uncal, ig_uncal, query_directions, candidates_uncal
#' )
#' samples_cal <- sample_radiometry(
#'   r_cal, ig_cal, query_directions, candidates_cal
#' )
#' # Inspect samples_uncal$points and samples_cal$points before fitting.
#' calibration <- crosscalibrate_radiometry(
#'   samples_uncal$measurements, samples_cal$measurements, degree = 3
#' )
#' calibration$reference_dn
#' calibration$profile_uncal
#'
#' ff_uncal <- flat_field_image(ig_uncal$off_axis, "simple_polynomial",
#'                              calibration$vignetting_coef)
#' corrected <- apply_flat_field(r_uncal, ff_uncal)
#' }
crosscalibrate_radiometry <- function(measurements_uncal, measurements_cal,
                                      degree = 5, k = 3) {
  .check_vector(degree, "integerish", 1, sign = "positive")
  if (degree > 6)
    stop("`degree` must not exceed 6 for simple_polynomial.", call. = FALSE)
  .check_vector(k, "integerish", 1, sign = "positive")
  prepare <- function(x, name) {
    .check_columns(x, required_columns = c("off_axis", "dn"),
      reserved_columns = c("reference_dn", "normalized_dn", "reference_weight"))
    .check_vector(x$off_axis, "numeric", sign = "nonnegative",
                  name = paste0(name, "$off_axis"))
    if (any(x$off_axis > pi))
      stop("`", name, "$off_axis` must not exceed pi radians.", call. = FALSE)
    .check_vector(x$dn, "numeric", sign = "positive", name = paste0(name, "$dn"))
    if (nrow(x) < k)
      stop("`", name, "` must contain at least `k` measurements.", call. = FALSE)
    if ("group" %in% names(x)) {
      group <- as.character(x$group)
      if (anyNA(group) || any(!nzchar(trimws(group))) || length(unique(group)) != 1L)
        stop("`", name, "` must contain exactly one nonmissing, nonempty group.",
             call. = FALSE)
    }
    x
  }
  profile_uncal <- prepare(measurements_uncal, "measurements_uncal")
  profile_cal <- prepare(measurements_cal, "measurements_cal")
  if (nrow(profile_uncal) != nrow(profile_cal))
    stop("The measurement tables must have the same number of rows.", call. = FALSE)

  normalize <- function(profile) {
    selected <- order(profile$off_axis, seq_len(nrow(profile)))[seq_len(k)]
    weights <- rcaiman::.inverse_distance_weights(profile$off_axis[selected])
    reference <- sum(weights * profile$dn[selected])
    if (!is.finite(reference) || reference <= 0)
      stop("The optical-axis reference must be finite and positive.", call. = FALSE)
    profile$reference_dn <- reference
    profile$reference_weight <- numeric(nrow(profile))
    profile$reference_weight[selected] <- weights
    profile$normalized_dn <- profile$dn / reference
    profile
  }
  profile_uncal <- normalize(profile_uncal)
  profile_cal <- normalize(profile_cal)
  ds <- data.frame(off_axis = profile_uncal$off_axis,
    off_axis_cal = profile_cal$off_axis, dn_uncal = profile_uncal$dn,
    dn_cal = profile_cal$dn, normalized_dn_uncal = profile_uncal$normalized_dn,
    normalized_dn_cal = profile_cal$normalized_dn)
  ds$relative_response <- ds$normalized_dn_uncal / ds$normalized_dn_cal
  .check_vector(ds$relative_response, "numeric", sign = "positive",
                name = "relative_response")
  fitted <- .fit_relative_response(ds, degree)
  c(list(ds = ds), fitted, list(profile_uncal = profile_uncal,
    profile_cal = profile_cal, reference_dn = c(uncal = profile_uncal$reference_dn[1],
                                               cal = profile_cal$reference_dn[1])))
}
