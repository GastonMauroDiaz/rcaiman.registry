#' @export
summary.hs_registry_entry <- function(object, ...) {

  .check_registry_entry(object)

  cat("\nRegistry entry - Summary\n")
  cat("========================\n\n")

  components <- object[names(object) != "instrument_metadata"]

  is_file     <- vapply(components, .is_file_sig, logical(1))
  is_embedded <- vapply(components, .is_embedded_metadata_sig, logical(1))

  file_sig_ids <- sort(names(components)[is_file])
  embedded_ids <- sort(names(components)[is_embedded])

  n_file_sig <- length(file_sig_ids)
  n_embedded <- length(embedded_ids)

  total_geometry   <- 0L
  total_radiometry <- 0L

  # -------------------------------------------------------------------------
  # File signatures
  # -------------------------------------------------------------------------

  cat(sprintf("File signatures (%d)\n", n_file_sig))
  cat("-------------------\n")

  if (n_file_sig > 0) {
    for (id in file_sig_ids) {
      cat(sprintf("* %s\n", id))
    }
  }

  cat("\n")

  # -------------------------------------------------------------------------
  # Embedded metadata signatures
  # -------------------------------------------------------------------------

  cat(sprintf("Embedded metadata signatures (%d)\n", n_embedded))
  cat("---------------------------------\n")

  for (ems in embedded_ids) {

    em <- components[[ems]]
    dim_val <- em$dim

    cat(sprintf("* %s\n", ems))
    cat("    Dimensions:\n")
    cat(sprintf("      - %d x %d\n", dim_val[1], dim_val[2]))

    geometry_ids <- character(0)

    if (!is.null(em$geometry) && is.list(em$geometry)) {
      geometry_ids <- sort(names(em$geometry))
    }

    n_geometry <- length(geometry_ids)
    total_geometry <- total_geometry + n_geometry

    cat(sprintf("    Geometry specifications (%d):\n", n_geometry))

    if (n_geometry > 0) {

      for (gs in geometry_ids) {

        g <- em$geometry[[gs]]

        radiometry_ids <- character(0)

        if (!is.null(g$radiometry) && is.list(g$radiometry)) {
          radiometry_ids <- sort(names(g$radiometry))
        }

        n_radiometry <- length(radiometry_ids)
        total_radiometry <- total_radiometry + n_radiometry

        cat(sprintf("      - %s\n", gs))

        if (n_radiometry > 0) {
          cat(sprintf("          Radiometry specifications (%d):\n", n_radiometry))
          for (rs in radiometry_ids) {
            cat(sprintf("            * %s\n", rs))
          }
        } else {
          cat("          Radiometry specifications: none\n")
        }
      }

    } else {
      cat("      none\n")
    }

    cat("\n")
  }

  invisible(object)
}
