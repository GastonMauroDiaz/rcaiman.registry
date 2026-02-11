#' Specs for Canon EOS 5D with Sigma 8mm fisheye
#'
#' @description
#' A complete set of geometric and radiometric specifications for a Canon EOS 5D
#' digital camera equipped with a Sigma 8mm F3.5 EX DG circular fisheye lens.
#'
#' @format
#' An object of class \code{hemispherical_camera_registry}.
#'
#' @details
#'
#' Centre coordinates were estimated from PGM files obtained via CR2
#' conversion using the open-source software
#' [DCRaw by Dave Coffin](https://dechifro.org/dcraw/) with the command:
#'
#' \preformatted{dcraw -4 -D}
#'
#' Image dimensions reported by different software are not identical:
#'
#' \itemize{
#'   \item EXIF metadata: 4368 × 2912
#'   \item dcraw:
#'     \itemize{
#'       \item Full size: 4476 × 2954
#'       \item Image size: 4386 × 2920
#'       \item Output size: 4386 × 2920
#'     }
#'   \item rawpy (use by \pkg{rcaiman}): Full size 4476 × 2954
#' }
#'
#' Centre coordinates derived from PGM images (4386 × 2920) were adjusted to
#' match the full raw sensor dimensions (4476 × 2954) used by \pkg{rcaiman}.
#'
#' The \code{zenith_colrow} argument incorporates this offset and its
#' rescaling by a factor of two to ensure compatibility with the spectral
#' bands obtained after the subsampling procedure described in
#' \insertCite{Lang2010;textual}{rcaiman.registry} and adopted in \pkg{rcaiman}.
#'
#' The original distortion model was defined in absolute pixel units
#' \insertCite{Lang2010}{rcaiman.registry}. For integration into \pkg{rcaiman},
#' the model was reformulated in terms of relative radius, with the intercept
#' removed and coefficients normalized with respect to the horizon radius.
#'
#' The residual standard error of the fitted model was 0.001746 (47 degrees of
#' freedom). The model evaluates to approximately `1` at a zenith angle of 90
#' deg, consistent with normalization to the horizon radius. The formulation has
#' been tested within \pkg{rcaiman}.
#'
#' @references \insertAllCited{}
#'
"Canon_EOS_5D.Sigma_8mm.EMY"
