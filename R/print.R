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
print.hemispherical_camera_registry <- function(x, ...) {

  cat("<hemispherical camera registry>\n\n")

  im <- x$instrument_metadata

  ## Identify configuration
  has_body   <- !is.null(im$body)
  has_device <- !is.null(im$device)
  has_lens   <- !is.null(im$lens)
  has_aux    <- !is.null(im$auxiliary_lens)

  config <- if (has_device) {
    "Device with fisheye lens"
  } else if (has_device && has_aux){
    "Device + auxiliary lens"
  }else if (has_body && has_aux) {
    "Compact camera + auxiliary lens"
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

  # Data added to the registry
  ## Split components by ontology
  registry_components <- .split_registry_components(x)

  cat("\nFile identity\n")
  cat("-------------\n")

  file_identity_ids <- registry_components$file_identity_ids

  if (length(file_identity_ids) == 0) {
    cat("No file identities registered\n")
  } else {
    cat(length(file_identity_ids), "file identity(ies)\n\n", sep = " ")
    for (eid in file_identity_ids) {
      cat("* ", eid, "\n", sep = "")
    }
  }

  cat("\nEmbedded metadata identity\n")
  cat("--------------------------\n")

  embedded_metadata_ids <- registry_components$embedded_metadata_ids

  if (length(embedded_metadata_ids) == 0) {
    cat("No embedded metadata identities registered\n")
  } else {
    cat(length(embedded_metadata_ids), "embeded metadata identity(ies)\n\n", sep = " ")
    for (eid in embedded_metadata_ids) {
      cat("* ", eid, "\n", sep = "")
    }
  }

  cat("\nSpecs\n")
  cat("-----\n")

  geom_ids <- registry_components$geometry_ids

  if (length(geom_ids) == 0) {
    cat("No geometry specs registered\n")
  } else {
    cat(length(geom_ids), "geometry spec(s)\n\n", sep = " ")
    for (gid in geom_ids) {
      g <- registry_components$components[[gid]]
      cat("* ", gid, "\n", sep = "")

      rad_ids <- names(g$radiometry)
      if (length(rad_ids) == 0) {
        cat("  Radiometry specs: none\n")
      } else {
        cat("  Radiometry specs:\n")
        for (rid in rad_ids) {
          cat("    - ", rid, "\n", sep = "")
        }
      }
    }
  }

  invisible(x)
}
