#' Specs for Nikon Coolpix 5700 with Nikon FC-E9 fisheye converter (CIEFAP)
#'
#' @description
#' A complete set of geometric and radiometric specifications for a Nikon Coolpix
#' 5700 digital compact camera equipped with a Nikon FC-E9 fisheye converter.
#'
#' @format
#' An object of class \code{hemispherical_camera_registry} containing
#' two geometry specification. Only one of them with an associated radiometry
#' specification.
#'
#' @details
#' Fig. 1 shows the Nikon Coolpix 5700 equipped with the Nikon FC-E9 fisheye
#' converter.
#'
#' \if{html}{\figure{Nikon_Coolpix5700.FCE9.CIEFAP.jpg}{options: style="display:block;margin:0 auto;max-width:70%;width:750px;"}}
#' \if{latex}{\figure{Nikon_Coolpix5700.FCE9.CIEFAP.jpg}}
#' _Figure 1. Photograph of the hemispherical camera system._
#'
#' More details about the geometry spec with `id = "delta-t jpeg"` can be found
#' in the supplementary materials of \insertCite{Diaz2018;textual}{rcaiman}.
#' It is valid for the JPEGs of maximum resolution produced by the
#' camera.
#'
#' Although both geometry and radiometry specs were obtained following the
#' methodology described in \insertCite{Diaz2024;textual}{rcaiman}, the
#' calibration parameters stored in this object differ from those reported in
#' the publication. The calibration parameters were revised in 2025, after
#' reverting the decision to replace the horizon radius returned by
#' [rcaiman::calibrate_lens()] with the one computed using
#' [rcaiman::calc_diameter()]. These parameters therefore differ from those
#' reported in \insertCite{Diaz2024;textual}{rcaiman}.
#'
#' @references \insertAllCited{}
#'
"Nikon_Coolpix5700.FCE9.CIEFAP"

