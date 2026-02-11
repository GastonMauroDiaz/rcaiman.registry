#' Add a radiometry spec to an existing geometry spec
#'
#' @description
#' Add a radiometry specification to an existing geometry
#' specification in a hemispherical camera registry.
#'
#' @details
#' A radiometry specification describes how pixel values produced by an
#' acquisition system should be interpreted or corrected from a radiometric
#' point of view.
#'
#' Radiometry specifications are always associated with a geometry
#' specification, and may serve two conceptually distinct purposes:
#'
#' \itemize{
#'   \item to define a quantitative radiometric correction model (e.g. vignetting),
#'   \item to declare interpretive constraints required to correctly understand
#'   the spectral meaning of the recorded bands or operational conventions
#'   imposed by downstream processing requirements, even when they do not
#'   correspond to a physically accurate spectral interpretation, such as when cyan
#'   is assigned to blue.
#' }
#'
#' The behavior and required arguments depend on the value of `type`.
#'
#' When `type = "vignetting_correction"`, the specification defines a parametric
#' radiometric model. Arguments `scheme` and `model_type` provide the semantic
#' context required by \pkg{rcaiman} to interpret `parameters` and apply the
#' corresponding correction. The effective mathematical model applied to images
#' is defined by [rcaiman::correct_vignetting()]. Interpretative constraints
#' are ignored.
#'
#' When `type = "interpretive_constraint"`, the specification declares
#' constraints or conventions that affect how sensor data must be interpreted.
#' At least one or these arguments must be provided: `cfa_pattern` and
#' `spectral_mapping`. Modeling-related arguments are ignored.
#'
#' @note
#' This function is declarative. It does not apply corrections or enforce
#' interpretations. Its role is to register expert knowledge about the
#' acquisition system in a form that can be inspected, audited, and
#' explicitly used by downstream processing code.
#'
#'
#' @param registry `hemispherical_camera_registry` object created with
#'   [new_registry()] and containing the geometry specification identified by
#'   `geometry_id`.
#' @param geometry_id character vector of length one. Identifier of the geometry
#'   specification to which the radiometry spec will be attached.
#' @param id character vector of length one. Identifier of the radiometry spec.
#'   Must use snake_case (lowercase letters, numbers, and underscores only)
#' @param type character vector of length one. See *Details*.
#' @param scheme character vector of length one. Calibration scheme defining
#'   the modeling assumptions used by the core for radiometric correction.
#'   The selected scheme determines the effective form of the model and the
#'   constraints (if any) applied to its parameters. Recognized schemes
#'   currently implemented in \pkg{rcaiman}: `"simple"` and `"free_form"`. This argument is intended for use with
#'   `type = "vignetting_correction"`.
#' @param model_type character vector of length one. Model type used to develop a
#'   radiometric correction. Only `"polynomial"` is currently implemented in
#'   \pkg{rcaiman}. This argument is intended for use with
#'   `type = "vignetting_correction"`.
#' @param parameters named list of model parameters. Model parameters defining
#'   the radiometric correction, following the conventions of the selected
#'   `model_type`. List names must be coercible to numeric values representing
#'   f-numbers. This argument is intended for use with
#'   `type = "vignetting_correction"`.
#' @param cfa_pattern character matrix of two rows by two columns. Declares the
#'   ordered set of color filter elements used by the sensor. This argument must
#'   be provided when `type = "interpretive_constraint"`. Values should
#'   correspond to semantic color labels (e.g. `"Red"`, `"Green"`, `"Blue"`,
#'   `"Yellow"`, `"Cyan"`, `"Magenta"`), and reflect the CFA pattern as produced
#'   by the sensor.
#' @param spectral_mapping optional named list. Declares how target spectral
#'   bands should be derived from sensor channels. This argument is intended for
#'   use with `type = "interpretive_constraint"`. Each element of the list must
#'   be named according to the target band (e.g. `"Red"`, `"Green"`, `"Blue"`).
#'   The corresponding value must be a function describing how that band is
#'   obtained from one or more spectral bands as declared in `cfa_pattern`. The
#'   formal arguments of each function define the source bands to be used and
#'   must match names in `cfa_pattern`. Functions must accept
#'   [`terra::SpatRaster`] objects as input and return a single-layer
#'   [`terra::SpatRaster`].
#' @param offset_value optional named list of black levels. List names must be coercible
#'   to numeric values representing ISO sensitivity.
#'
#' @inheritParams add_geometry_spec
#'
#' @return
#' A `hemispherical_camera_registry` object with the radiometry specification
#' appended to the selected geometry specification.
#'
#' @export
#'
#' @examples
#' foo <- new_registry(
#'   "Nikon_D610.Nikkor_8mm.CIEFAP",
#'   body = "D610",
#'   body_manufacturer = "NIKON CORP",
#'   body_serial = "9023728",
#'   lens = "AF-S FISHEYE NIKKOR 8-15mm 1:3.5-4.5E ED",
#'   lens_manufacturer = "NIKON CORP",
#'   lens_serial = "210020",
#'   institution = "CIEFAP"
#' )
#'
#' foo <- add_geometry_spec(
#'   foo,
#'   id = "simple_method",
#'   lens_coef = signif(c(1306,24.8,-56.2)/1894,3),
#'   zenith_colrow = c(1500, 997),
#'   horizon_radius = 946,
#'   is_horizon_circle_clipped = FALSE,
#'   max_zenith_angle = 92.8,
#'   dim = c(3040, 2014),
#'   firmware_version = "1.01",
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' foo <- add_radiometry_spec(
#'   foo,
#'   geometry_id = "simple_method",
#'   id = "simple_method",
#'   type = "vignetting_correction",
#'   scheme = "simple",
#'   model_type = "polynomial",
#'   parameters = list("3.5" = c(0.0302, 0.320, 0.0908)),
#'   firmware_version = "1.01",
#'   notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
#'
#' foo <- add_radiometry_spec(
#'   foo,
#'   geometry_id = "simple_method",
#'   id = "spectral_bands",
#'   type = "interpretive_constraint",
#'   cfa_pattern = matrix(c("Red","Green1",
#'                          "Green2","Blue"), byrow = TRUE, ncol = 2),
#'   spectral_mapping = list(Red = function(Red) Red,
#'                           Green = function(Green1, Green2) mean(Green1, Green2),
#'                           Blue = function(Blue) Blue),
#'   firmware_version = "1.01",
#'   contact_information = "gastonmaurodiaz@gmail.com"
#' )
add_radiometry_spec <- function(
    registry,
    geometry_id,
    id,
    type,
    scheme = NULL,
    model_type = NULL,
    parameters = NULL,
    cfa_pattern = NULL,
    spectral_mapping = NULL,
    offset_value = NULL,
    date = NULL,
    firmware_version = NULL,
    notes = NULL,
    contact_information = NULL
) {

  if (is.null(date)) {
    date <- Sys.Date()
  }

  if (!inherits(registry, "hemispherical_camera_registry")) {
    stop(
      "`registry` must be an object of class 'hemispherical_camera_registry'.",
      call. = FALSE
    )
  }

  .check_vector(geometry_id, "character", 1)
  if (is.null(registry[[geometry_id]])) {
    stop(
      sprintf("Geometry spec with id '%s' does not exist in this registry.", geometry_id),
      call. = FALSE
    )
  }

  geometry <- registry[[geometry_id]]
  if (is.null(geometry$radiometry)) {
    geometry$radiometry <- list()
  }

  if (!is.null(geometry$radiometry[[id]])) {
    stop(
      sprintf(
        "Radiometry spec with id '%s' already exists for geometry '%s'.",
        id, geometry_id
      ),
      call. = FALSE
    )
  }


  .check_vector(type, "character", 1)
  .assert_choice(type, c("vignetting_correction", "interpretive_constraint"))

  if (type == "vignetting_correction") {
    .check_vector(scheme, "character", 1)
    .assert_choice(scheme, c("simple", "free-form", "legacy"))
    if (scheme != "legacy") {
      .check_vector(model_type, "character", 1)
      .assert_choice(model_type, "polynomial")
      if (!is.list(parameters)) {
        stop("`parameters` must be a list.")
      }
      nms <- names(parameters)
      if (is.null(nms) || any(nms == "")) {
        stop("`parameters` must be a named list with names representing aperture values.")
      }
      aperture_num <- suppressWarnings(as.numeric(nms))
      if (any(is.na(aperture_num))) {
        stop("All `parameters` names must be coercible to numeric aperture values (f-numbers).")
      }
      if (any(!is.finite(aperture_num)) || any(aperture_num <= 0)) {
        stop("Aperture values in `parameters` must be positive and finite.")
      }
      if (any(duplicated(aperture_num))) {
        stop("Duplicated aperture values detected in `parameters`.")
      }
      ok <- vapply(
        parameters,
        function(x) is.numeric(x) && length(x) >= 1,
        logical(1)
      )
      if (!all(ok)) {
        stop("Each element of `parameters` must be a numeric vector of length >= 1.")
      }
    } else {
      if (any(!is.null(model_type), !is.null(parameters))) {
        stop("`scheme´= 'legacy'`, `model_type` and `parameters` should be `NULL`")
      }
    }
  }
  if (type == "interpretive_constraint") {
    if (!is.null(offset_value)) {

      if (!is.list(offset_value)) {
        stop("`offset_value` must be a list.")
      }
      nms <- names(offset_value)
      if (is.null(nms) || any(nms == "")) {
        stop("`offset_value` must be a named list with names representing ISO sensitivity values.")
      }
      aperture_num <- suppressWarnings(as.numeric(nms))
      if (any(is.na(aperture_num))) {
        stop("All `offset_value` names must be coercible to numeric ISO sensitivity values.")
      }
      if (any(!is.finite(aperture_num)) || any(aperture_num <= 0)) {
        stop("ISO sensitivity values in `offset_value` must be positive and finite.")
      }
      if (any(duplicated(aperture_num))) {
        stop("Duplicated ISO sensitivity values detected in `offset_value`.")
      }
      ok <- vapply(
        offset_value,
        function(x) is.numeric(x) && length(x) == 1,
        logical(1)
      )
      if (!all(ok)) {
        stop("Each element of `offset_value` must be a numeric vector of length one.")
      }
    }

    if (is.null(cfa_pattern)) {
      stop(
        "For `type = 'interpretive_constraint'`, `cfa_pattern` must be provided.",
        call. = FALSE
      )
    }
    if (!is.matrix(cfa_pattern) || !is.character(cfa_pattern)) {
      stop("`cfa_pattern` must be a character matrix.")
    }
    if (length(unique(as.vector(cfa_pattern))) != 4) {
      stop(
        "`cfa_pattern` must define exactly four unique band identifiers.",
        call. = FALSE
      )
    }
    if (!is.null(spectral_mapping)) {
      if (!is.list(spectral_mapping) ||
          is.null(names(spectral_mapping)) ||
          any(names(spectral_mapping) == "") ||
          any(!vapply(spectral_mapping, is.function, logical(1)))) {
        stop("spectral_mapping must be a named list of functions.")
      }
      for (nm in names(spectral_mapping)) {

        fun <- spectral_mapping[[nm]]

        fmls <- formals(fun)
        args <- names(fmls)

        # no dots
        if ("..." %in% args) {
          stop(
            "Functions in `spectral_mapping` must not use `...` (found in '", nm, "').",
            call. = FALSE
          )
        }

        # number of arguments
        if (length(args) < 1 || length(args) > 4) {
          stop(
            "Function '", nm,
            "' must have between 1 and 4 arguments.",
            call. = FALSE
          )
        }

        # argument names must match cfa_pattern
        if (!all(args %in% cfa_pattern)) {
          stop(
            "Function '", nm,
            "' uses undefined bands: ",
            paste(setdiff(args, cfa_pattern), collapse = ", "),
            call. = FALSE
          )
        }
      }
    }

  }

  .check_vector(date, "date", 1)
  .check_vector(firmware_version, "character", 1, allow_null = TRUE)
  .check_vector(notes, "character", 1, allow_null = TRUE)
  .check_vector(contact_information, "character", 1, allow_null = TRUE)

  radiometry_spec <- list(
    id = id,
    type = type,
    scheme = scheme,
    model_type = model_type,
    parameters = parameters,
    date = date,
    cfa_pattern = cfa_pattern,
    spectral_mapping = spectral_mapping,
    offset_value = offset_value,
    firmware_version = firmware_version,
    notes = notes,
    contact_information = contact_information
  )

  registry[[geometry_id]]$radiometry[[id]] <- radiometry_spec

  registry
}
