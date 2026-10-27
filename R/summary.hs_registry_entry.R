#' @export
summary.hs_registry_entry <- function(object, ...) {
  idx <- inspect_registry_index(object)$signature
  cat("\nRegistry entry - Summary\n")
  cat("========================\n\n")
  cat(sprintf("Signatures (%d)\n", length(idx)))
  cat("---------------------------------\n")
  for (ems in sort(names(idx))) {
    em <- idx[[ems]]
    cat(sprintf("* %s\n", ems))
    cat("    Dimensions:\n")
    cat(sprintf("      - %d x %d\n", em$dim[1], em$dim[2]))
    cat(sprintf("    Band specifications (%d):\n", length(em$band_spec_ids)))
    if (!length(em$band_spec_ids)) cat("      none\n")
    for (bs in sort(em$band_spec_ids)) {
      band <- em$band_spec[[bs]]
      cat(sprintf("      - %s\n", bs))
      cat(sprintf("          Geometry specifications (%d):\n",
                  length(band$geometry_spec_ids)))
      if (!length(band$geometry_spec_ids)) cat("            none\n")
      for (gs in sort(band$geometry_spec_ids)) {
        rad_ids <- band$radiometry_spec_ids[[gs]]
        cat(sprintf("            - %s\n", gs))
        if (length(rad_ids)) {
          cat(sprintf("                Radiometry specifications (%d):\n",
                      length(rad_ids)))
          for (rs in sort(rad_ids)) cat(sprintf("                  * %s\n", rs))
        } else {
          cat("                Radiometry specifications: none\n")
        }
      }
    }
    cat("\n")
  }
  invisible(object)
}
