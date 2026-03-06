#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom graphics points
#' @importFrom magrittr %>%
#' @importFrom stats coefficients
#' @importFrom stats lm
#' @importFrom stats splinefun
#' @importFrom terra rast
#' @importFrom terra vect
## usethis namespace: end
NULL

#' @import rcaiman
NULL

# https://groups.google.com/g/rdevtools/c/qT6cJt6DLJ0
# spurious importFrom to avoid note
#' @importFrom Rdpack c_Rd
NULL

# https://github.com/tidyverse/magrittr/issues/29
#' @importFrom utils globalVariables
NULL
utils::globalVariables(c("."))
