#' @export
summary.hemispherical_camera_registry <- function(object, ...) {

  x <- object

  cat("Summary of hemispherical camera registry\n")
  cat("========================================\n\n")

  im <- x$instrument_metadata

  cat("Instrument ID: ", im$id, "\n", sep = "")

  geom_ids <- setdiff(names(x), "instrument_metadata")

  cat("\nGeometry specifications: ", length(geom_ids), "\n\n", sep = "")

  if (length(geom_ids) == 0) {
    cat("No geometry specifications registered\n")
    return(invisible(x))
  }

  for (gid in geom_ids) {

    g <- x[[gid]]

    cat("Geometry specification: ", gid, "\n", sep = "")

    if (!is.null(g$dim)) {
      cat("  Valid for raster dimension: ", g$dim[1], " x ", g$dim[2], "\n", sep = "")
    }

    rad <- g$radiometry
    rad_ids <- names(rad)

    if (length(rad_ids) == 0) {
      cat("  Radiometry specifications: none\n\n")
      next
    }

    cat("  Radiometry specifications: ", length(rad_ids), "\n\n", sep = "")

    for (rid in rad_ids) {

      r <- rad[[rid]]

      cat("  Radiometry specification: ", rid, "\n", sep = "")
      cat("    Type:   ", r$type, "\n", sep = "")
      cat("    Scheme: ", r$scheme, "\n", sep = "")
      cat("    Model type:  ", r$model_type, "\n", sep = "")

      params <- r$parameters

      if (is.list(params) && length(params) > 0) {

        ap <- suppressWarnings(as.numeric(names(params)))
        ord <- order(ap)

        ap <- ap[ord]
        params <- params[ord]

        cat("\n")
        cat("    Aperture-dependent parameters:\n")
        cat("      Available apertures (f-number): ",
            paste(format(ap, trim = TRUE), collapse = ", "),
            "\n", sep = "")
        cat("      Number of parameter sets: ", length(ap), "\n", sep = "")
        cat("      Number of coefficients per aperture:\n")

        for (i in seq_along(ap)) {
          cat(
            "        f/",
            format(ap[i], trim = TRUE),
            " : ",
            length(params[[i]]),
            "\n",
            sep = ""
          )
        }
      }

      cat("\n")
    }
  }

  invisible(x)
}
