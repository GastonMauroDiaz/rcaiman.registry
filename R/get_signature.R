#' Extract signature
#'
#' @description
#' Retrieve a signature within a registry entry.
#'
#' @param signature character vector of length one. Identifier of
#'   signature.
#'
#' @inheritParams add_signature
#'
#' @export
#'
#' @examples
#' # See add_signature()
get_signature <- function(registry_entry, signature) {


  .check_registry_entry(registry_entry)
  .check_vector(signature, "character", 1)

  if (!signature %in% names(registry_entry) ||
      !.is_signature(registry_entry[[signature]])) {
    stop(
      "Signature '",
      signature,
      "' not found in this registry entry"
    )
  }

  registry_entry[[signature]]
}
