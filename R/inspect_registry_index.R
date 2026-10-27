#' Inspect registry entry structure
#'
#' @description
#' Return a hierarchical index of the components stored in a
#' registry entry.
#'
#' @inheritParams add_signature
#'
#' @return
#' named list with `signature` entries. Each contains `dim`,
#' `band_spec_ids`, and a `band_spec` list indexed by band identifier. Each
#' band entry contains `geometry_spec_ids` and `radiometry_spec_ids`, the
#' latter indexed by geometry identifier. Empty branches have no identifiers.
#'
#' @export
#'
#' @examples
#' entry <- read_registry_snapshot(system.file(
#'   "external/my_entry_v2.yaml", package = "rcaiman.registry"
#' ))
#' entry
#' summary(entry)
#' inspect_registry_index(entry)
inspect_registry_index <- function(registry_entry) {
  .check_registry_entry(registry_entry)
  components <- registry_entry[names(registry_entry) != "instrument_metadata"]
  embedded <- components[vapply(components, .is_signature, logical(1))]
  embedded_index <- lapply(embedded, function(em) {
    .check_signature(em)
    bands <- lapply(em$bands, function(band) {
      .check_band_spec(band)
      list(
        geometry_spec_ids = as.character(names(band$geometry)),
        radiometry_spec_ids = lapply(band$geometry, function(g) {
          as.character(names(g$radiometry))
        })
      )
    })
    list(
      dim = em$dim,
      band_spec_ids = as.character(names(em$bands)),
      band_spec = bands
    )
  })
  list(signature = embedded_index)
}
