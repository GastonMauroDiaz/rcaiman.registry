#' Extract a band specification
#'
#' @description
#' Retrieve band interpretation from a registry entry or a metadata signature.
#'
#' @param x [hs_registry_entry] or [signature] object.
#' @param signature character vector of length one. Metadata
#'   signature identifier, required when `x` is a registry entry.
#' @param band_spec character vector of length one. Band specification
#'   identifier within the selected metadata signature.
#' @param ... additional arguments (unused).
#'
#' @return [band_spec] object.
#'
#' @seealso [add_band_spec()], [get_geometry_spec()]
#' @export
#' @examples
#' # See add_band_spec()
get_band_spec <- function(x, ...) {
  UseMethod("get_band_spec")
}

#' @rdname get_band_spec
#' @export
get_band_spec.hs_registry_entry <- function(x, signature,
                                            band_spec, ...) {
  em <- get_signature(x, signature)
  get_band_spec(em, band_spec = band_spec)
}

#' @rdname get_band_spec
#' @export
get_band_spec.signature <- function(x, band_spec, ...) {
  .check_signature(x)
  .check_vector(band_spec, "character", 1)
  if (!band_spec %in% names(x$bands)) {
    stop("Band specification '", band_spec,
         "' not found under embedded metadata '", x$id, "'.", call. = FALSE)
  }
  result <- x$bands[[band_spec]]
  .check_band_spec(result)
  result
}
