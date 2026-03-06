.show_popup <- function(message) {
  # Create the main window
  win <- tcltk::tktoplevel()
  tcltk::tkwm.title(win, "Popup")

  # Create a label with the message
  label <- tcltk::tklabel(win, text = message, padx = 10,
                          pady = 10, justify = "left")
  tcltk::tkpack(label)

  # Create a button to close the window
  button <- tcltk::tkbutton(win, text = "OK",
                            command = function() tcltk::tkdestroy(win))
  tcltk::tkpack(button, padx = 10, pady = 10)

  # Set the focus on the window
  tcltk::tkfocus(win)

  # Run the event loop
  tcltk::tkwait.window(win)
}

.is_embedded_metadata_sig <- function(x) {
  inherits(x, "embedded_metadata_sig")
}

.is_file_sig <- function(x) {
  inherits(x, "file_sig")
}

.split_registry_components <- function(registry_entry) {
  stopifnot(inherits(registry_entry, "hs_registry_entry"))

  components <- registry_entry[names(registry_entry) != "instrument_metadata"]

  is_embedded <- vapply(components, .is_embedded_metadata_sig, logical(1))
  is_file     <- vapply(components, .is_file_sig, logical(1))

  embedded_ids <- names(components)[is_embedded]
  file_ids     <- names(components)[is_file]

  ## Extract geometry specs nested inside embedded metadata
  geometry_ids <- character(0)
  geometry_map <- list()

  for (eid in embedded_ids) {

    em <- components[[eid]]

    if (!is.null(em$geometry) && is.list(em$geometry)) {

      g_ids <- names(em$geometry)

      for (gid in g_ids) {
        geometry_ids <- c(geometry_ids, gid)
        geometry_map[[gid]] <- em$geometry[[gid]]
      }
    }
  }

  list(
    components            = components,
    file_signature_ids    = file_ids,
    embedded_metadata_ids = embedded_ids,
    geometry_ids          = geometry_ids,
    geometry_map          = geometry_map
  )
}


