#' Registry entry for a hemispherical imaging system
#'
#' @description
#' Objects of class [hs_registry_entry] describe instrument
#' specifications according with the requirements of \pkg{rcaiman} core
#' functions. They store structured data organized into descriptive and
#' specification domains.
#'
#' @details
#' This is an S3 class implemented as a named list. Its purpose is to make
#' explicit, inspectable, and auditable the geometric and radiometric
#' assumptions applied during canopy image preprocessing. This, in turn,
#' requires the correct identification of the photographic system and its
#' configuration state at the moment of acquisition. To that end, three distinct
#' but related descriptive domains are represented in a registry entry:
#'   \itemize{
#'     \item instrument metadata, which describes the physical acquisition system
#'     as a real-world object,
#'     \item file signature ([file_sig]), and
#'     \item embedded metadata signature ([embedded_metadata_sig]).
#'   }
#'
#' These domains are intentionally separated. A physical instrument exists
#' independently of any particular file;  file-level properties describe how
#' image data are materialized as files by the acquisition system; and embedded
#' metadata act as a trace of a specific acquisition event.
#'
#' A registry entry must contain one instrument metadata entry, which defines the
#' physical acquisition system. For all other domains multiple entries are
#' allowed, reflecting differences in firmware versions, acquisition settings,
#' or calibration procedures.
#'
#' In practice, image data labeled as *raw* may be affected by in-camera
#' processing options (e.g. illumination or distortion corrections) that alter
#' how values are stored, even when the file format remains nominally *raw*. The
#' applicability of a given geometric or radiometric specification is therefore
#' conditional on identity signatures derived from observable metadata. The
#' registry entry makes these conditional dependencies explicit: what is known to
#' depend on configuration is formally declared.
#'
#' The registry entry assumes that configuration states relevant for image
#' interpretation are detectable through embedded metadata. However, the extent
#' to which manufacturers expose internal processing steps is not fully known.
#' Consequently, the registry entry formalizes only those conditions that are
#' observable and declared.
#'
#' The relation between specifications and signatures is structurally defined
#' within a registry entry.
#'
#' Specifications are considered valid only under equality of the declared
#' identity conditions. Determining when it is scientifically justified to relax
#' such conditions is outside the scope of the registry entry and remains a
#' matter of expert judgment.
#'
#' Registry entries can be distributed through R packages. The
#' `rcaiman.registry.data` package provides a curated set of official entries,
#' while users and research groups can create their own packages following,
#' preferably, the convention `rcaiman.registry.<name>` to distribute custom
#' registry entries.
#'
#' @section Structure:
#' The object is a named list:
#'
#' \if{html}{\figure{hs_registry_entry.png}{options: style="display:block;margin:0 auto;max-width:100%;width:750px;"}}
#' \if{latex}{\figure{hs_registry_entry.png}}
#'
#' @note
#' Objects stored in a registry entry are assumed to be technically valid under their
#' declared identity and configuration state conditions.
#'
#' @seealso
#' \itemize{
#' \item [new_registry_entry()] to create a new [hs_registry_entry] object with the mandatory element `instrument_metadata`
#' \item [add_file_sig()] to add a [file_sig] to an existing registry entry
#' \item [add_embedded_metadata_sig()] to add a [embedded_metadata_sig] to an existing registry entry
#' \item [add_geometry_spec()] to add a [geometry_spec] to an existing registry entry
#' \item [add_radiometry_spec()] to add a [radiometry_spec] to an existing registry entry
#' }
#'
#' @name hs_registry_entry
#' @docType class
NULL

#' File signature
#'
#' @description
#' Represent a file signature used by \pkg{rcaiman} to facilitate
#' file retrieval tasks.
#'
#' @section Definition:
#' A `file_sig` describes image files as observed externally (e.g. filename
#' structure, extension).
#'
#' @section Domain of application:
#' A file signature acquires declared validity only within a
#' [hs_registry_entry].
#'
#' @section Structure:
#' A `file_sig` is an S3 object with the following fields:
#'
#' \describe{
#'   \item{id}{Identifier (snake_case).}
#'   \item{extension}{Expected file extension (e.g., `"NEF"`, `"CR2"`).}
#'   \item{filename_pattern}{Optional regular expression describing expected basename structure.}
#'   \item{date}{Date of signature declaration.}
#' }
#'
#' @section Generation:
#' A file signature is created via [add_file_sig()]. Registration is declarative
#' and does not perform file matching.
#'
#' @section Retrieving:
#' A `file_sig` may be retrieved using [get_file_sig()].
#'
#' @note
#' Can be used independently to supply arguments to \pkg{rcaiman} core
#' functions. However, its domain of validity remains tied as declared in
#' *Domain of application*.
#'
#' @name file_sig
#' @docType class
NULL

#' Embedded metadata signature
#'
#' @description
#' Represent an embedded metadata signature used by \pkg{rcaiman} to
#' identify the instruments that captured an image and their configuration
#' state at acquisition time.
#'
#' @section Definition:
#' An `embedded_metadata_sig` represents a configuration state of an
#' instrument as expressed through embedded metadata.
#'
#' @section Domain of application:
#' An embedded metadata signature acquires full meaning only within a
#' [hs_registry_entry] via `intrument_metada`, which purpouse is to
#' offers keys to identify the p physical acquisition system that can produce
#' such data.
#'
#' @section Structure:
#'
#' An `embedded_metadata_sig` is an S3 object with the following fields:
#'
#' \describe{
#'   \item{id}{Identifier (snake_case).}
#'   \item{namespace}{Lowercase identifier of metadata namespace (e.g., `"exif"`).}
#'   \item{dim}{Integer-like vector of length two specifying image width and height.}
#'   \item{rules}{Named list of expected tag–value pairs.}
#'   \item{date}{Date of signature declaration.}
#'   \item{file_sig}{Optional foreign key referencing a [file_sig] of the [hs_registry_entry] object.}
#'   \item{geometry}{Named list of [geometry_spec] objects.}
#' }
#'
#' @section Generation and validation:
#'
#' An embedded metadata signature is created via [add_embedded_metadata_sig()].
#'
#' Registration validates structural consistency (identifier format, rule
#' structure, optional file signature linkage), but does not perform metadata
#' matching at creation time.
#'
#' @section Retrieving:
#' A `embedded_metadata_sig` may be retrieved using [get_embedded_metadata_sig()].
#'
#' @note
#' Can be used independently to supply arguments to \pkg{rcaiman} core
#' functions. However, its domain of validity remains tied as declared in
#' *Domain of application*.
#'
#' @name embedded_metadata_sig
#' @docType class
NULL

#' Geometry specification
#'
#' @description
#' Represent a geometry specification defining the geometric reference frame
#' used by \pkg{rcaiman} to interpret directional information from an image.
#'
#' @section Definition:
#' A `geometry_spec` is a formal declaration of the geometric transformation
#' between image coordinates and spherical coordinates (zenith and azimuth)
#' under a specific instrument configuration.
#'
#' @section Domain of application:
#' A `geometry_spec` acquires declared validity only when associated with an
#' [embedded_metadata_sig] within a [hs_registry_entry]. Its
#' position in that hierarchy determines the metadata context under which the
#' geometric interpretation is applicable.
#'
#' @section Structure:
#'
#' A `geometry_spec` is an S3 object with the following fields:
#'
#' \describe{
#'   \item{id}{Identifier (snake_case).}
#'   \item{model}{Character vector identifying the geometric model used to
#'   define the image-to-sphere transformation.}
#'   \item{parameters}{Named list containing the parameters required
#'   by the declared geometric model.}
#'   \item{max_zenith_angle}{Optional numeric value specifying the maximum
#'   observable zenith angle (deg).}
#'   \item{date}{Date of specification declaration.}
#'   \item{notes}{Optional free-form documentation.}
#'   \item{contact_information}{Optional contact information for traceability.}
#'   \item{radiometry}{Named list of [radiometry_spec] objects.}
#' }
#'
#' For `model = "radial_projection"`, `parameters` must include:
#'
#' \describe{
#'   \item{projection_coef}{Numeric vector defining the lens projection model.}
#'   \item{zenith_col_row}{Numeric vector of length 2 specifying the optical
#'   center in pixel coordinates.}
#'   \item{horizon_radius}{Numeric value specifying the pixel radius
#'   corresponding to 90° zenith angle.}
#'   \item{is_horizon_circle_clipped}{Logical value indicating whether the
#'   horizon circle is clipped by the image matrix.}
#' }
#'
#' The specific radial projection model implemented in \pkg{rcaiman} is
#' described in [rcaiman::calc_relative_radius()].
#'
#' Other model types may require different parameter structures, but none have
#' been implemented yet in \pkg{rcaiman}.
#'
#' @section Generation and validation:
#'
#' A `geometry_spec` is created via [add_geometry_spec()].
#'
#' For `model = "radial_projection"`, structural constraints imposed by
#' \pkg{rcaiman} are enforced at creation time, including validation of
#' projection coefficients via [rcaiman::validate_radial_projection_function()].
#'
#' Additional contextual validation is performed when the specification is
#' inserted into a registry. In particular, if `is_horizon_circle_clipped =
#' FALSE`, the extrapolated horizon circle (90° zenith angle) must lie entirely
#' within the image dimensions (`dim`) declared in the associated
#' [embedded_metadata_sig]. If this condition is not satisfied, registration
#' fails.
#'
#' @section Retrieving:
#' A `geometry_spec` may be retrieved using [get_geometry_spec()].
#'
#' @note
#' Can be used independently to supply arguments to \pkg{rcaiman} core
#' functions. However, its domain of validity remains tied as declared in
#' *Domain of application*.
#'
#' @seealso [calibrate_lens()], [crosscalibrate_lens()], [calc_diameter()]
#'
#' @name geometry_spec
#' @docType class
NULL

#' Radiometry specification
#'
#' @description
#' Represent a radiometry specification defining the interpretative
#' constraints and radiometric correction operators used by \pkg{rcaiman}
#' to obtain radiometrically consistent data from imaging systems.
#'
#' @section Definition:
#' A `radiometry_spec` is a formal declaration of radiometric interpretation
#' under a specific geometric interpretation of an imaging system.
#'
#' Radiometry specifications serve one of two purposes:
#'
#' * to define a quantitative flat-field correction model;
#' * to declare interpretive constraints required to correctly understand
#'   spectral meaning or operational conventions.
#'
#' @section Domain of application:
#' A `radiometry_spec` acquires declared validity only when associated with a
#' [geometry_spec] within a [hs_registry_entry]. Its structural
#' position determines the geometric and metadata context under which the
#' radiometric interpretation applies.
#'
#' @section Structure:
#'
#' A `radiometry_spec` is an S3 object with the following fields:
#'
#' \describe{
#'   \item{id}{Identifier (snake_case).}
#'   \item{type}{Either `"flat_field_correction"` or `"interpretive_constraint"`.}
#'   \item{model}{Character vector identifying the flat-field model (applicable
#'    when `type = "flat_field_correction"`).}
#'   \item{parameters}{Named list of model parameters (applicable when
#'    `type = "flat_field_correction"`).}
#'   \item{data}{Named list of dense flat-field data (applicable when
#'    `type = "flat_field_correction"`).}
#'   \item{cfa_pattern}{Character matrix defining CFA structure (applicable
#'    when `type = "interpretive_constraint"`).}
#'   \item{spectral_mapping}{Named list of functions mapping sensor channels
#'    to target bands (applicable when `type = "interpretive_constraint"`).}
#'   \item{offset_value}{Optional named list of black level values (applicable
#'    when `type = "interpretive_constraint"`).}
#'   \item{date}{Date of specification declaration.}
#'   \item{notes}{Optional free-form documentation.}
#'   \item{contact_information}{Optional contact information for traceability.}
#' }
#'
#' @section Generation and validation:
#'
#' A `radiometry_spec` is created within a
#' [hs_registry_entry] via [add_radiometry_spec()].
#'
#' Structural validation depends on `type`. For flat-field models,
#' the declared function must satisfy a strict structural contract
#' (see [add_radiometry_spec()] for details). For interpretive
#' constraints, CFA structure and spectral mapping consistency
#' are validated at registration time.
#'
#' A radiometry specification is declarative. It does not apply
#' corrections or enforce interpretation. Its role is to register
#' expert knowledge in a structured and inspectable form.
#'
#' @section Retrieving:
#' A `radiometry_spec` may be retrieved using [get_radiometry_spec()].
#'
#' @note
#' Can be used independently to supply arguments to \pkg{rcaiman} core
#' functions. However, its domain of validity remains tied as declared in
#' *Domain of application*.
#'
#' @seealso [extract_radiometry()]
#'
#' @name radiometry_spec
#' @docType class
NULL
