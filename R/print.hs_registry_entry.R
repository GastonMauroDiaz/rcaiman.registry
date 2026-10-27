.print_component <- function(title, x, indent = "  ") {
  if (is.null(x)) return(invisible(NULL))

  cat(title, ":\n", sep = "")

  if (!is.list(x)) {
    cat(indent, as.character(x), "\n", sep = "")
    return(invisible(NULL))
  }

  if (!is.null(x$model)) {
    cat(indent, "Model:        ", x$model, "\n", sep = "")
  }

  if (!is.null(x$manufacturer)) {
    cat(indent, "Manufacturer: ", x$manufacturer, "\n", sep = "")
  }

  if (!is.null(x$serial)) {
    cat(indent, "Serial:       ", x$serial, "\n", sep = "")
  }

  invisible(NULL)
}


#' @export
print.hs_registry_entry <- function(x, ...) {

  cat("<Registry entry>\n\n")

  ## -------------------------------------------------------------------------
  ## Instrument metadata
  ## -------------------------------------------------------------------------

  im <- x$instrument_metadata

  ## Identify configuration
  has_body   <- !is.null(im$body)
  has_device <- !is.null(im$device)
  has_lens   <- !is.null(im$lens)
  has_aux    <- !is.null(im$auxiliary_lens)

  config <- if (has_device && has_aux) {
    "Device + auxiliary lens"
  } else if (has_device) {
    "Device with integrated fisheye"
  } else if (has_body && has_aux) {
    "Camera body + auxiliary lens"
  } else if (has_body && has_lens) {
    "Camera body + interchangeable lens"
  } else {
    "Unknown configuration"
  }

  cat("Instrument system\n")
  cat("-----------------\n")
  cat("ID:            ", im$id, "\n", sep = "")
  cat("Configuration: ", config, "\n", sep = "")

  if (!is.null(im$institution)) {
    cat("Institution:   ", im$institution, "\n", sep = "")
  }

  .print_component("Device", im$device)
  .print_component("Camera body", im$body)
  .print_component("Lens", im$lens)
  .print_component("Auxiliary lens", im$auxiliary_lens)


  ## -------------------------------------------------------------------------
  ## Structural overview
  ## -------------------------------------------------------------------------

  idx <- inspect_registry_index(x)$signature
  cat("\nRegistry contents\n")
  cat("-----------------\n")
  cat("Signatures: ", length(idx), "\n", sep = "")
  total_bands <- total_geometry <- total_radiometry <- 0L
  if (!length(idx)) cat("No signatures registered\n")
  for (ems in names(idx)) {
    em <- idx[[ems]]
    total_bands <- total_bands + length(em$band_spec_ids)
    cat("\n* ", ems, "\n", sep = "")
    cat("    Band specifications: ", length(em$band_spec_ids), "\n", sep = "")
    for (bs in em$band_spec_ids) {
      band <- em$band_spec[[bs]]
      total_geometry <- total_geometry + length(band$geometry_spec_ids)
      cat("      - ", bs, " (geometry: ", length(band$geometry_spec_ids),
          ")\n", sep = "")
      for (gs in band$geometry_spec_ids) {
        n_rad <- length(band$radiometry_spec_ids[[gs]])
        total_radiometry <- total_radiometry + n_rad
        cat("          - ", gs, " (radiometry: ", n_rad, ")\n", sep = "")
      }
    }
  }
  cat("\nTotal band specifications: ", total_bands, "\n", sep = "")
  cat("Total geometry specifications: ", total_geometry, "\n", sep = "")
  cat("Total radiometry specifications: ", total_radiometry, "\n", sep = "")
  invisible(x)
}
