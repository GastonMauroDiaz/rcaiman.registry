#' Extract radiometry specification
#'
#' @description
#' Retrieve a radiometry specification associated with a geometry specification
#' within a registry entry or not.
#'
#' @param x Either:
#'   - A [hs_registry_entry], or
#'   - A [geometry_spec].
#' @param signature Character vector of length one.
#'   Identifier of signature. Required when `x` is a
#'   `hs_registry_entry`.
#' @param band_spec character vector of length one. Band specification
#'   identifier, required when `x` is a registry entry.
#' @param geometry_spec Character vector of length one.
#'   Identifier of geometry specification. Required when `x` is a
#'   `hs_registry_entry`.
#' @param radiometry_spec Character vector of length one.
#'   Identifier of radiometry specification.
#' @param ... Additional arguments (unused).
#'
#' @return
#' An object of class [radiometry_spec].
#'
#' @details
#' When `x` is a [hs_registry_entry], the function navigates the hierarchy:
#'
#' `registry_entry, signature, band_spec, geometry_spec, radiometry_spec`
#'
#' When `x` is a [geometry_spec], the radiometry specification is retrieved
#' directly from its `radiometry` container.
#'
#' @seealso
#' [add_radiometry_spec()], [get_geometry_spec()], [calibrate_radiometry()]
#'
#' @export
#'
#' @examples
#' # See add_radiometry_spec()
get_radiometry_spec <- function(x, ...) {
  UseMethod("get_radiometry_spec")
}

#' @rdname get_radiometry_spec
#' @export
get_radiometry_spec.hs_registry_entry <- function(x,
                                                  signature,
                                                  band_spec,
                                                  geometry_spec,
                                                  radiometry_spec,
                                                  ...) {

  .check_registry_entry(x)
  .check_vector(signature, "character", 1)
  .check_vector(geometry_spec, "character", 1)
  .check_vector(radiometry_spec, "character", 1)

  g <- get_geometry_spec(
    x = x,
    signature = signature,
    band_spec = band_spec,
    geometry_spec = geometry_spec
  )

  get_radiometry_spec(g, radiometry_spec = radiometry_spec)
}

#' @rdname get_radiometry_spec
#' @export
get_radiometry_spec.geometry_spec <- function(x,
                                              radiometry_spec,
                                              ...) {

  .check_geometry_spec(x)
  .check_vector(radiometry_spec, "character", 1)

  if (!radiometry_spec %in% names(x$radiometry)) {
    stop(
      "Radiometry specification '",
      radiometry_spec,
      "' not found under geometry '",
      x$id,
      "'"
    )
  }

  x$radiometry[[radiometry_spec]]
}
