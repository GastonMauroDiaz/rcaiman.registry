#' @export
summary.hemispherical_camera_registry <- function(object, ...) {

  x <- object

  cat("Summary of hemispherical camera registry\n")
  cat("========================================\n\n")

  ## Instrument metadata
  im <- x$instrument_metadata
  cat("Instrument ID: ", im$id, "\n", sep = "")

  ## Split components by ontology
  registry_components <- .split_registry_components(x)
  geom_ids <- registry_components$geometry_ids
  embedded_metadata_ids <- registry_components$embedded_metadata_ids
  file_identity_ids <- registry_components$file_identity_ids

  ## File identities
  cat("\nFile identities: ", length(file_identity_ids), "\n", sep = "")
  if (length(file_identity_ids) == 0) {
    cat("  None registered\n")
  }

  ## Embedded metadata identities
  cat("\nEmbedded metadata identities: ", length(embedded_metadata_ids), "\n", sep = "")
  if (length(embedded_metadata_ids) == 0) {
    cat("  None registered\n")
  }

  ## Geometry specifications
  cat("\nGeometry specifications: ", length(geom_ids), "\n\n", sep = "")

  if (length(geom_ids) == 0) {
    cat("No geometry specifications registered\n")
    return(invisible(x))
  }

  for (gid in geom_ids) {

    g <- registry_components$components[[gid]]

    cat("Geometry specification: ", gid, "\n", sep = "")

    if (!is.null(g$dim)) {
      cat(
        "  Valid for raster dimension: ",
        g$dim[1], " x ", g$dim[2], "\n",
        sep = ""
      )
    }

    ## Radiometry
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
      cat("    Type:       ", r$type, "\n", sep = "")
      cat("    Scheme:     ", r$scheme, "\n", sep = "")

      if (!is.null(r$model_type)) {
        cat("    Model type: ", r$model_type, "\n", sep = "")
      }

      params <- r$parameters

      if (is.list(params) && length(params) > 0) {

        ap <- suppressWarnings(as.numeric(names(params)))
        ord <- order(ap)

        ap <- ap[ord]
        params <- params[ord]

        cat("\n")
        cat("    Aperture-dependent parameters:\n")
        cat(
          "      Available apertures (f-number): ",
          paste(format(ap, trim = TRUE), collapse = ", "),
          "\n", sep = ""
        )
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
