.new_embedded_metadata_sig <- function(
    id,
    namespace,
    rules,
    dim,
    date = NULL,
    file_sig = NULL
) {

  .check_vector(id, "character", 1)
  .assert_id(id)
  .check_vector(namespace, "character", 1)
  .assert_id(namespace)
  .check_vector(dim, "integerish", length = 2)
  if (length(rules) == 0) {
    stop("`rules` must contain at least one tag expectation.", call. = FALSE)
  }
  if (!is.list(rules) || is.null(names(rules))) {
    stop(
      "`rules` must be a named list of tag expectations.",
      call. = FALSE
    )
  }
  if (anyDuplicated(names(rules))) {

    dup <- unique(names(rules)[duplicated(names(rules))])

    stop(
      sprintf(
        "Duplicated rule names are not allowed: %s",
        paste(dup, collapse = ", ")
      ),
      call. = FALSE
    )

  }
  # check leading / trailing whitespace in values
  nm <- names(rules)
  bad <- vapply(
    rules,
    function(x) is.character(x) && grepl("^\\s|\\s$", x),
    logical(1)
  )

  if (any(bad)) {
    offenders <- sprintf("%s = '%s'", nm[bad], rules[bad])
    stop(
      sprintf(
        "Rule values contain leading or trailing whitespace: %s",
        paste(offenders, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  .check_vector(date, "date", 1)
  .check_vector(file_sig, "character", 1, allow_null = TRUE)

  x <- list(
    id = id,
    namespace = namespace,
    dim = dim,
    rules = rules,
    date = date,
    file_sig = file_sig,
    geometry = list()
  )
  class(x) <- "embedded_metadata_sig"
  x
}

#' Add an embedded metadata signature to an existing registry
#'
#' @description
#' Add a [embedded_metadata_sig] to a [hs_registry_entry] object.
#'
#' @param registry_entry [hs_registry_entry] object created with
#'   [new_registry_entry()].
#' @param id character vector of length one. Identifier of embedded metadata signature.
#'   Must use snake_case (lowercase letters, numbers, and underscores only)
#' @param namespace character vector of length one. Lowercase identifier of the
#'   embedded metadata namespace (e.g. "exif", "xmp"). This value is descriptive
#'   and does not trigger any validation against `rules` at registration time.
#' @param dim integer-like numeric vector of length two. Width and height in
#'   pixels of the raster from which `zenith_col_row` was derived.
#' @param rules named list composed of character vectors of length one. Expected
#'   metadata values. Names must correspond to tag names.
#' @param date optional [`Date`] object. Date when the specification was added
#'   to the registry. If `NULL`, the current system date is used automatically.
#' @param file_sig optional character vector of length one.
#'   Intended to support workflow at the file retrieval stage. When provided,
#'   the registry validates the presence of the declared file
#'   signature at registration time.
#'
#' @return
#' A [hs_registry_entry] object with the [embedded_metadata_sig] object appended.
#'
#' @export
#'
#' @examples
#' foo <- new_registry_entry(
#'   "Nikon_Coolpix5700.FCE9.CIEFAP",
#'   body = "E5700",
#'   body_manufacturer = "NIKON CORP",
#'   body_serial = "7053067",
#'   lens = "Zoom Nikkor ED 8.9-71.2mm 1:2.8-4.2",
#'   lens_manufacturer = "NIKON CORP",
#'   auxiliary_lens = "Fisheye Converter FC-E9 0.2x",
#'   auxiliary_lens_manufacturer = "NIKON CORP",
#'   institution = "CIEFAP"
#' )
#'
#' foo <- add_file_sig(
#'   foo,
#'   id = "raw",
#'   extension = "NEF",
#'   filename_pattern = "^DSCN[0-9]{4}$"
#' )
#'
#' foo <- add_embedded_metadata_sig(
#'   foo,
#'   id = "exif_01",
#'   namespace = "exif",
#'   dim = c(1288, 962),
#'   rules = list(
#'     "Camera Model Name" = "E5700",
#'     "Software" = "E5700v1.1",
#'     "CFA Pattern" = "[Yellow,Cyan][Green,Magenta]",
#'     "Compression" = "Uncompressed",
#'     "Bits Per Sample" = "12",
#'     "Quality" = "RAW"
#'   ),
#'   file_sig = "raw"
#' )
#'
#' foo
#' summary(foo)
add_embedded_metadata_sig <- function(
    registry_entry,
    id,
    namespace,
    rules,
    dim,
    date = NULL,
    file_sig = NULL
) {

  .check_registry_entry(registry_entry)
  if (is.null(date)) {
    date <- Sys.Date()
  }
  if (!is.null(registry_entry[[id]])) {
    stop(
      sprintf("An embedded metadata signature with id '%s' already exists in this registry entry.", id),
      call. = FALSE
    )
  }
  .check_vector(file_sig, "character", 1, allow_null = TRUE)
  if (!is.null(file_sig)) {
    if (is.null(registry_entry[[file_sig]])) {
      stop(
        "The proposed file signature does not exist in this registry entry."
      )
    }
    .check_file_sig(registry_entry[[file_sig]])
  }

  embedded_metadata_sig <- .new_embedded_metadata_sig(
    id = id,
    namespace = namespace,
    dim = dim,
    rules = rules,
    date = date,
    file_sig = file_sig
  )

  registry_entry[[id]] <- embedded_metadata_sig
  registry_entry
}
