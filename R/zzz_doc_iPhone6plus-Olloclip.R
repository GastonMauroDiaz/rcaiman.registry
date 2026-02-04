#' Specs for iPhone 6 Plus (rear camera) with Olloclip auxiliary lens
#'
#' @description
#' A complete set of geometric and radiometric specifications for an iPhone 6
#' Plus (rear camera) with Olloclip fisheye auxiliary lens.
#'
#' @format
#' An object of class \code{hemispherical_camera_registry} containing
#' one geometry specification and one associated radiometry specification.
#'
#' @details
#' Fig. 1 shows the iPhone 6 Plus with the Olloclip auxiliary lens attached to its rear camera.
#'
#' \if{html}{\figure{iPhone6plus.Olloclip.jpg}{options: style="display:block;margin:0 auto;max-width:70%;width:750px;"}}
#' \if{latex}{\figure{iPhone6plus.Olloclip.jpg}}
#' _Figure 1. Photograph of the hemispherical camera system._
#'
#' Both geometry and radiometry specs were obtained
#' following the methodology described in
#' \insertCite{Diaz2024;textual}{rcaiman}. Those calibration parameters were
#' revised in 2025, when the decision of replacing the horizon radius returned
#' by [rcaiman::calibrate_lens()] by the one calculated with
#' [rcaiman::calc_diameter()] was overturned.
#'
#' The optical design principles of the auxiliary lens are described in patent US9454066B2.
#'
#' @references \insertAllCited{}
#'
"iPhone6plus.Olloclip"
