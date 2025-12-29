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

  cat("\nComponents\n")
  cat("----------\n")

  .print_component("Device", im$device)
  .print_component("Camera body", im$body)
  .print_component("Lens", im$lens)
  .print_component("Auxiliary lens", im$auxiliary_lens)

  ## Geometry specs
  geom_ids <- setdiff(names(x), "instrument_metadata")

  cat("\nGeometry specs\n")
  cat("--------------\n")

  if (length(geom_ids) == 0) {
    cat("No geometry specs registered\n")
  } else {
    cat(length(geom_ids), "geometry spec(s)\n\n", sep = " ")
    for (gid in geom_ids) {
      g <- x[[gid]]
      n_rad <- length(g$radiometry)

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
