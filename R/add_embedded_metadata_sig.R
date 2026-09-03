.validate_metadata_rules <- function(rules) {
  if (!is.list(rules) || length(rules) == 0 || is.null(names(rules))) {
    stop("`rules` must be a non-empty named list.", call. = FALSE)
  }

  ids <- names(rules)
  if (anyDuplicated(ids)) {
    stop("Duplicated rule identifiers are not allowed.", call. = FALSE)
  }
  if (any(!grepl("^[a-z][a-z0-9_]*$", ids))) {
    stop(
      "Rule identifiers must use snake_case and start with a letter.",
      call. = FALSE
    )
  }

  required <- c("group", "tag", "label", "expected")
  keys <- character(length(rules))
  for (i in seq_along(rules)) {
    rule <- rules[[i]]
    if (!is.list(rule) || !identical(sort(names(rule)), sort(required))) {
      stop(
        sprintf(
          "Rule '%s' must contain exactly: group, tag, label, expected.",
          ids[[i]]
        ),
        call. = FALSE
      )
    }
    for (field in required) {
      value <- rule[[field]]
      if (!is.character(value) || length(value) != 1 || is.na(value) ||
          !nzchar(value)) {
        stop(
          sprintf("Rule '%s' field '%s' must be non-empty text.", ids[[i]], field),
          call. = FALSE
        )
      }
      if (trimws(value) != value) {
        stop(
          sprintf(
            "Rule '%s' field '%s' contains leading or trailing whitespace.",
            ids[[i]], field
          ),
          call. = FALSE
        )
      }
    }
    keys[[i]] <- paste(rule$group, rule$tag, sep = ":")
  }

  if (anyDuplicated(keys)) {
    duplicated_keys <- unique(keys[duplicated(keys)])
    stop(
      sprintf(
        "Duplicated ExifTool group + tag keys are not allowed: %s",
        paste(duplicated_keys, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(rules)
}

.new_embedded_metadata_sig <- function(
    id,
    rules,
    dim,
    validated_with_libraw,
    validated_with_exiftool,
    metadata_profile = "exiftool_g1_short_v1",
    date = NULL
) {
  .check_vector(id, "character", 1)
  .assert_id(id)
  .check_vector(dim, "integerish", length = 2)
  .check_vector(validated_with_libraw, "character", allow_na = TRUE)
  .check_vector(validated_with_exiftool, "character", allow_na = TRUE)
  .assert_choice(metadata_profile, "exiftool_g1_short_v1")
  .validate_metadata_rules(rules)
  .check_vector(date, "date", 1)

  x <- list(
    id = id,
    dim = dim,
    validated_with_libraw = validated_with_libraw,
    validated_with_exiftool = validated_with_exiftool,
    metadata_profile = metadata_profile,
    rules = rules,
    date = date,
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
#'   pixels of the raster that have been validated to be compatible with the
#'   specifications associated with this `embedded_metadata_sig`.
#' @param validated_with_libraw character vector. Version(s) of the *LibRaw*
#'   library that have been validated to be compatible with the
#'   specifications associated with this `embedded_metadata_sig`. Multiple
#'   values allow recording additional verified versions without overwriting
#'   previously validated ones. Use `NA` for the signature of JPEG files.
#' @param validated_with_exiftool character vector. ExifTool version(s) that
#'   have been validated to be compatible with the specifications associated
#'   with this `embedded_metadata_sig`. Multiple values allow recording
#'   additional verified versions without overwriting previously validated ones.
#'   `NA` is allowed.
#' @param metadata_profile Character vector. Identify how image metadata
#'   were extracted and named. Currently the only supported value is
#'   `"exiftool_g1_short_v1"`, which means ExifTool output with Family 1
#'   groups and short technical tag names. This field is stored explicitly
#'   so future extraction profiles can be distinguished safely.
#' @param rules named list of embedded metadata identity rules. Each rule name
#'   is a stable snake_case identifier, and each rule must be a list with
#'   exactly four character fields: `group`, `tag`, `label`, and `expected`.
#'   `group` and `tag` identify the ExifTool metadata item, such as
#'   `"IFD0"` and `"Model"`; `label` is a human-readable name; and
#'   `expected` is the value that must match when validating an image.
#'   Duplicated `group` + `tag` pairs are not allowed. These rules may be
#'   written by hand or assembled from a metadata rules draft produced by
#'   [rcaiman-projects](https://github.com/GastonMauroDiaz/rcaiman-projects) via
#'    the "Build metadata rules..." action.
#' @param date optional [`Date`] object. Date when the specification was added
#'   to the registry. If `NULL`, the current system date is used automatically.
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
#' #Rules written by hand.
#' foo <- add_embedded_metadata_sig(
#'   foo,
#'   id = "exif_raw",
#'   dim = c(1288, 962),
#'   validated_with_libraw = "0.21.1",
#'   validated_with_exiftool = "13.59",
#'   rules = list(
#'     auto_focus = list(
#'       group = "Composite",
#'       tag = "AutoFocus",
#'       label = "AutoFocus",
#'       expected = "On"
#'     ),
#'     software = list(
#'       group = "IFD0",
#'       tag = "Software",
#'       label = "Software",
#'       expected = "E5700v1.1"
#'     ),
#'     digital_zoom = list(
#'       group = "Nikon",
#'       tag = "DigitalZoom",
#'       label = "DigitalZoom",
#'       expected = "1"
#'     ),
#'     quality = list(
#'       group = "Nikon",
#'       tag = "Quality",
#'       label = "Quality",
#'       expected = "RAW"
#'     )
#'   )
#' )
#'
#' #Alternatively, rules from a metadata rules draft built with rcaiman-projects
#' path <- system.file("external/nikon_coolpix5700_fce9_ciefap_snippet.R",
#'                      package = "rcaiman.registry")
#' source(path)
#' metadata_rules
#'
#' foo
#' summary(foo)
add_embedded_metadata_sig <- function(
    registry_entry,
    id,
    rules,
    dim,
    validated_with_libraw,
    validated_with_exiftool,
    metadata_profile = "exiftool_g1_short_v1",
    date = NULL
) {
  .check_registry_entry(registry_entry)
  if (is.null(date)) {
    date <- Sys.Date()
  }
  if (!is.null(registry_entry[[id]])) {
    stop(
      sprintf(
        "An embedded metadata signature with id '%s' already exists.",
        id
      ),
      call. = FALSE
    )
  }

  registry_entry[[id]] <- .new_embedded_metadata_sig(
    id = id,
    rules = rules,
    dim = dim,
    validated_with_libraw = validated_with_libraw,
    validated_with_exiftool = validated_with_exiftool,
    metadata_profile = metadata_profile,
    date = date
  )
  registry_entry
}
