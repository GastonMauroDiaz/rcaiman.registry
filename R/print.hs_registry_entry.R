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

  idx <- inspect_registry_index(x)

  n_file <- length(idx$file_sig_ids)
  ems_ids <- names(idx$embedded_metadata_sig)
  n_embedded <- length(ems_ids)

  cat("\nRegistry contents\n")
  cat("-----------------\n")
  cat("File signatures: ", n_file, "\n", sep = "")
  cat("Embedded metadata signatures: ", n_embedded, "\n", sep = "")

  total_geometry <- 0
  total_radiometry <- 0

  if (n_embedded == 0) {
    cat("No embedded metadata signatures registered\n")
    return(invisible(x))
  }

  for (ems in ems_ids) {

    em_struct <- idx$embedded_metadata_sig[[ems]]
    g_ids <- em_struct$geometry_spec_ids
    n_geom <- length(g_ids)

    total_geometry <- total_geometry + n_geom

    cat("\n* ", ems, "\n", sep = "")
    cat("    Geometry specifications: ", n_geom, "\n", sep = "")

    for (gs in g_ids) {

      rad_ids <- em_struct$radiometry_spec_ids[[gs]]
      n_rad <- length(rad_ids)

      total_radiometry <- total_radiometry + n_rad

      cat("      - ", gs,
          " (radiometry: ", n_rad, ")",
          "\n", sep = "")
    }
  }

  cat("\nTotal geometry specifications: ", total_geometry, "\n", sep = "")
  cat("Total radiometry specifications: ", total_radiometry, "\n", sep = "")

  invisible(x)
}
