#' Extract geometry specification
#'
#' @description
#' Retrieve a geometry specification from its band interpretation,
#' within a registry entry or retrieved separately.
#'
#' @param x Either:
#'   - A [hs_registry_entry], or
#'   - A [signature], or
#'   - A [band_spec].
#' @param signature Character vector of length one.
#'   Identifier of signature. Required when `x`
#'   is a `hs_registry_entry`.
#' @param band_spec character vector of length one. Band specification
#'   identifier, required unless `x` is a [band_spec].
#' @param geometry_spec Character vector of length one.
#'   Identifier of geometry specification.
#' @param ... Additional arguments (unused).
#'
#' @return
#' An object of class [geometry_spec].
#'
#' @details
#' When `x` is a [hs_registry_entry], the function navigates the hierarchy:
#'
#' `registry_entry, signature, band_spec, geometry_spec`
#'
#' From a [signature], select a band specification first.
#' From a [band_spec], retrieve geometry directly from its `geometry` container.
#'
#' @seealso
#' [add_geometry_spec()], [get_band_spec()], [get_radiometry_spec()]
#'
#' @export
#'
#' @examples
#' # See add_geometry_spec()
get_geometry_spec <- function(x, ...) {
  UseMethod("get_geometry_spec")
}

#' @rdname get_geometry_spec
#' @export
get_geometry_spec.hs_registry_entry <- function(x, signature,
                                                band_spec, geometry_spec, ...) {
  band <- get_band_spec(x, signature, band_spec)
  get_geometry_spec(band, geometry_spec = geometry_spec)
}

#' @rdname get_geometry_spec
#' @export
get_geometry_spec.signature <- function(x, band_spec,
                                  geometry_spec, ...) {
  band <- get_band_spec(x, band_spec = band_spec)
  get_geometry_spec(band, geometry_spec = geometry_spec)
}

#' @rdname get_geometry_spec
#' @export
get_geometry_spec.band_spec <- function(x, geometry_spec, ...) {
  .check_band_spec(x)
  .check_vector(geometry_spec, "character", 1)
  if (!geometry_spec %in% names(x$geometry)) {
    stop("Geometry specification '", geometry_spec,
         "' not found under band specification '", x$id, "'.", call. = FALSE)
  }
  x$geometry[[geometry_spec]]
}
