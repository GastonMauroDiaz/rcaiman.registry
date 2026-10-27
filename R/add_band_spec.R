.new_band_spec <- function(id, cfa_pattern = NULL, spectral_mapping = NULL,
                           date, notes = NULL, contact_information = NULL) {
  x <- structure(list(
    id = id,
    cfa_pattern = cfa_pattern,
    spectral_mapping = spectral_mapping,
    date = date,
    notes = notes,
    contact_information = contact_information,
    geometry = list()
  ), class = "band_spec")
  .check_band_spec(x)
  x
}

#' Add a band specification
#'
#' @description
#' Declare band interpretation under a signature.
#'
#' @details
#' A band specification contains the CFA pattern and spectral mapping, when
#' applicable, and groups the geometry specifications that use this
#' interpretation. Its place in the registry describes dependencies; it does
#' not prescribe whether spectral mapping precedes a radiometric correction.
#'
#' Both band fields may be omitted. Supply a CFA pattern when providing a
#' spectral mapping, so its expressions can be checked against the source
#' band identifiers. An omitted mapping does not have special meaning.
#'
#' @inheritParams add_signature
#' @inheritParams add_geometry_spec
#' @param id character vector of length one. Identifier of the band
#'   specification, unique within the selected metadata signature.
#' @param cfa_pattern optional character matrix of size 2 x 2 with four
#'   distinct source band identifiers and no missing values.
#' @param spectral_mapping optional named list of character expressions.
#'   Names identify target bands. Expressions use source band identifiers
#'   from `cfa_pattern`, numeric constants, and `+`, `-`, `*`, `/`, and
#'   parentheses.
#'
#' @return [hs_registry_entry] with the new [band_spec] appended.
#'
#' @seealso [get_band_spec()], [add_geometry_spec()]
#' @export
#' @examples
#' my_entry <- new_registry_entry(
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
#' path <- system.file("external/nikon_coolpix5700_fce9_ciefap_snippet.R",
#'                      package = "rcaiman.registry")
#' source(path)
#' metadata_rules
#'
#' my_entry <- add_signature(
#'   my_entry,
#'   id = "raw",
#'   dim = c(1288, 962),
#'   validated_with_libraw = "0.21.1",
#'   validated_with_exiftool = "13.59",
#'   rules = metadata_rules
#' )
#'
#' my_entry <- add_band_spec(
#'   my_entry,
#'   signature = "raw",
#'   id = "cygm",
#'   cfa_pattern =  matrix(c("Green", "Yellow",
#'                           "Magenta", "Cyan"), byrow = TRUE, ncol = 2),
#'   spectral_mapping = list(Red = "Magenta", Green = "Green", Blue = "Cyan")
#' )
#'
#' get_band_spec(my_entry, "raw", "cygm")
#' summary(my_entry)
add_band_spec <- function(registry_entry,
                          signature,
                          id,
                          cfa_pattern = NULL,
                          spectral_mapping = NULL,
                          date = NULL,
                          notes = NULL,
                          contact_information = NULL) {

  em <- get_signature(registry_entry, signature)
  .check_signature(em)
  .check_vector(id, "character", 1)
  .assert_id(id)
  if (id %in% names(em$bands)) {
    stop("Band specification '", id, "' already exists under embedded metadata '",
         signature, "'.", call. = FALSE)
  }
  if (is.null(date)) date <- Sys.Date()
  registry_entry[[signature]]$bands[[id]] <- .new_band_spec(
    id, cfa_pattern, spectral_mapping, date, notes, contact_information
  )
  registry_entry
}
