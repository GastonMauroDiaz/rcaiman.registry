#' Specs for
#'
#' @description
#' A complete set of geometric and radiometric specifications for
#'
#' @format
#' An object of class \code{hemispherical_camera_registry} containing
#' one geometry specification and one associated radiometry specification.
#'
#' @details
#'
#' \insertCite{Diaz2024;textual}{rcaiman}.
#'
#'
#'
#' @references \insertAllCited{}
#'
"Canon_EOS_5D.Sigma_8mm.EMY"



#' Fisheye image centre on raw data. The centre is determined from full matrix, but data are needed for extracted blue pixels according to pattern
#' The Centre was estimated from pgm files using CR2 conversion with  dcraw -4 -D,
#' then the image is  "Image size:  4386 x 2920" (there is something else in exif!: Image Size                      : 4368x2912) ,
#' but rawpy reads "Full size:   4476 x 2954". So, the offset must be added.
#' Check dcraw -i -v and also check images in rcaiman with plot() before processing.
#' dcraw -i -v
#' Thumb size:  2496 x 1664
#' Full size:   4476 x 2954
#' Image size:  4386 x 2920
#' Output size: 4386 x 2920
#' A extracted pgm header looks like :
#' % head -c 200 IMG_0186.pgm
#' P5
#' 4386 2920
#' 65535
#'
#' Coefficients for relative distance geometry model to convert data into equidistant projection.
#' bdistortion_fnc <- function(z){
#' HSP-s kasutatav mudel on vaja teha ümber suhtelise raadiuse peale.
#' return (973.389*z+60.160*(z^2)-75.316*( z^3))
#'> bdistortion_fnc(90*pi/180 )
#'[1] 1385.526
#'  09.10.2025 sai lähendatud mudel suhtelise kauguse peale
#' vabaliiget ei ole. Parameetrid võinuks ka lihtsalt raadiusega jagada :).
#'Formula: kaugus ~ a * nurkrad + b * nurkrad^2 + c * nurkrad^3
#'Parameters:
#'   Estimate Std. Error t value Pr(>|t|)
#'a  0.702301   0.002885  243.41  < 2e-16 ***
#'b  0.043405   0.005149    8.43 5.85e-11 ***
#'c -0.054341   0.002198  -24.72  < 2e-16 ***
#'---
#'Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#'
#'Residual standard error: 0.001746 on 47 degrees of freedom
#' return(0.702301*z + 0.043405*(z^2)-0.054341*(z^3) )
#'bdistortion_fnc(90*pi/180 )
#'[1] 0.9996554
#' Parameetrid on rcaimanis testitud. }
#' EMÜ Canon EOS 5D +Sigma 8 mm mudel. See on küll sinise kanali järgi, aga ehk võib siin ignoreerida spektraalset sõltuvust.
#' Old combined model that was used also in HSP. TBD: Separate polynomials for every aperture may provide some increase in accuracy.
