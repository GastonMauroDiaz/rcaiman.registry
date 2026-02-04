#' Specs for Canon EOS 6D Mark II with Zenitar 8mm
#'
#' @description
#' A complete set of geometric and radiometric specifications for Canon EOS 6D
#' Mark II with MC-Zenitar 8mm.
#'
#' @format
#' An object of class \code{hemispherical_camera_registry} containing
#' two geometry specification. Only one of them with an associated radiometry
#' specification.
#'
#' @details
#' Both geometry and radiometry specs tagged as "simple_method" were obtained
#' following the methodology described in
#' \insertCite{Diaz2024;textual}{rcaiman}.
#'
#' The lens apperture of the Zenitar 8mm can be mechanically set at f/22, f/16, f/11, f/8, f/5.6,
#' f/4, and f/3.5 (The value is not stored in the image metadata).
#'
#' The simple method was carried out at f/22. Fig. 1 and Fig. 2 show the resulting models.
#'
 #' \if{html}{\figure{Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE_1.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE_1.jpg}}
#' _Figure 1. Geometric data (the data points) taken with the simple method and the
#' Canon EOS 6D Mark II with the Zenitar 8mm set to f/22 and the model fit to this
#' data (the solid line)_
#'
#' \if{html}{\figure{Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE_2.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE_2.jpg}}
#' _Figure 2. Radiometric data (the data points) taken with the simple method and the
#' Canon EOS 6D Mark II with the Zenitar 8mm set to f/22 and the model fit to this
#' data (the solid line)_
#'
#' Data were collected to evaluate the effect of aperture on lens vignetting.
#' The camera was mounted pointing upward under open-sky, cloud-free conditions.
#' Without moving the camera, photographs were acquired across all available
#' apertures using a fixed shutter speed (1/2500) and automatic ISO sensitivity,
#' which ranged from 2500 at f/22 to 100 at f/3.5. Direct light was prevented
#' from entering the lens by casting a shadow over the lens pupil using a small
#' disk.
#'
#' Images were captured as quickly as possible to preserve lighting conditions
#' relative to the reference aperture (f/22). Figure 3 shows the relative
#' in-camera radiance of the blue band computed from concentric rings of
#' one-degree width. A mask was applied to exclude sky regions near the solar
#' disk.
#'
#' As shown in Fig. 3, apertures from f/22 to f/5.6 exhibit only minor variation.
#' Consequently, the model fitted at f/22 can be applied within this range with
#' negligible loss of accuracy.

#'
#' \if{html}{\figure{Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE_3.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE_3.jpg}}
#' _Figure 3. Uncorrected Relative radiance of a clear sky measured with
#' the Canon EOS 6D Mark II with the Zenitar 8mm._
#'
#'
#' @references \insertAllCited{}
#'
"Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE"
