#' Specs for Nikon D610 with Nikkor 8–15mm fisheye (CIEFAP)
#'
#' @description
#' A complete set of geometric and radiometric specifications for a Nikon D610
#' digital camera equipped with a Nikkor 8–15mm fisheye lens, fixed at 8 mm.
#'
#' @format
#' An object of class \code{hemispherical_camera_registry}.
#'
#' @details
#' Fig. 1 shows the Nikon D610 digital camera equipped with the Nikkor 8–15mm
#' fisheye lens fixed at 8 mm.
#'
#' \if{html}{\figure{Nikon_D610.Nikkor_8mm.CIEFAP.jpg}{options: style="display:block;margin:0 auto;max-width:70%;width:750px;"}}
#' \if{latex}{\figure{Nikon_D610.Nikkor_8mm.CIEFAP.jpg}}
#' _Figure 1. Photograph of the hemispherical camera system._
#'
#' Both geometry and radiometry specs tagged as "simple_method" were obtained
#' following the methodology described in
#' \insertCite{Diaz2024;textual}{rcaiman.registry}. Those calibration parameters are the
#' reported in the publication.
#'
#' Since the simple method was carried out at f/3.5, a cross-calibration was
#' employed to obtain the vignetting correction parameters for the other
#' apertures supported by the lens. The camera was located pointing upwards
#' under the open sky at dusk and in cloud-free conditions. Without moving the
#' camera, photographs were taken in aperture priority mode to cover the
#' available aperture range as quickly as possible. This was done to ensure the
#' same light conditions as the reference aperture (f/3.5). Fig. 2 shows the
#' inside-camera relative radiance of the blue band from rings of one degree
#' width.
#'
#'
#' \if{html}{\figure{Nikon_D610.Nikkor_8mm.CIEFAP_2.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{Nikon_D610.Nikkor_8mm.CIEFAP_2.jpg}}
#' _Figure 2. Uncorrected Relative radiance of a clear sky at dusk measured with
#' the Nikon D610 digital camera equipped with a Nikkor 8–15mm fisheye lens
#' fixed at 8 mm, in aperture priority mode._
#'
#' Fig. 3 shows the uncorrected normalized data up to 70 degrees of zenith
#' angle since at low angles mountains and trees obscured the sky. The results
#' of the cross-calibration was stored in the object.
#'
#' \if{html}{\figure{Nikon_D610.Nikkor_8mm.CIEFAP_3.jpg}{options: style="display:block;margin:0 auto;max-width:90%;width:750px;"}}
#' \if{latex}{\figure{Nikon_D610.Nikkor_8mm.CIEFAP_3.jpg}}
#' _Figure 3. Uncorrected relative radiance normalized with the corrected
#' relative radiance and polynomial models fitted to the data._
#'
#' @references \insertAllCited{}
#'
"Nikon_D610.Nikkor_8mm.CIEFAP"

