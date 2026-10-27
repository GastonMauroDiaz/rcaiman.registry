#' Registry entry for a hemispherical imaging system
#'
#' @description
#' Objects of class [hs_registry_entry] describe an imaging system and its
#' specifications according to the requirements of \pkg{rcaiman} core
#' functions. They store structured data organized into descriptive and
#' specification domains.
#'
#' @details
#' In this registry, "hemispherical imaging system" refers to an imaging system
#' characterized so that its observations can be mapped onto a hemispherical
#' representation. The term describes the intended domain of representation,
#' not the angular coverage of an individual acquisition. An acquisition may
#' cover only part of the hemisphere, and partial coverage does not require
#' additional acquisitions to complete it.
#'
#' The geometric specification describes directions relative to the optical
#' axis. The orientation of each acquisition places those directions in the
#' surrounding hemisphere. Mapping observations to a hemispherical raster is
#' a separate processing choice.
#'
#' This is an S3 class implemented as a named list. Its purpose is to make
#' explicit, inspectable, and auditable the geometric and radiometric
#' assumptions applied during canopy image preprocessing. This, in turn,
#' requires the correct identification of the photographic system and its
#' configuration state at the moment of acquisition. To that end, two distinct
#' but related descriptive domains are represented in a registry entry:
#'   \itemize{
#'     \item instrument metadata, which describes the physical acquisition system
#'     as a real-world object, and
#'     \item signature ([signature]).
#'   }
#'
#' These domains are intentionally separated. A physical instrument exists
#' independently of any particular file, while an embedded
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
#' Registry entries can be distributed through R packages.
#' \pkg{rcaiman.registry.data} provides a curated set of official entries,
#' while users and research groups can create their own packages following,
#' preferably, the convention `rcaiman.registry.<name>` to distribute custom
#' registry entries.
#'
#' @section Structure:
#' The object is a named list:
#'
#' \preformatted{
#' hs_registry_entry
#' +-- instrument_metadata (unique, mandatory element)
#' +-- signature (1...n, indexed by a user-defined ID)
#'     +-- bands (named container): band_spec (1...n, indexed by a user-defined ID)
#'         +-- geometry (named container): geometry_spec (1...n, indexed by a user-defined ID)
#'             +-- radiometry (named container): radiometry_spec (1...n, indexed by a user-defined ID)
#' }
#'
#' @note
#' Objects stored in a registry entry are assumed to be technically valid under their
#' declared identity and configuration state conditions.
#'
#' @seealso
#' \itemize{
#' \item [new_registry_entry()] to create a new [hs_registry_entry] object with the mandatory element `instrument_metadata`
#' \item [add_signature()] to add a [signature] to an existing registry entry
#' \item [add_band_spec()] to add a [band_spec] under a metadata signature
#' \item [add_geometry_spec()] to add a [geometry_spec] to an existing registry entry
#' \item [add_radiometry_spec()] to add a [radiometry_spec] to an existing registry entry
#' }
#'
#' @name hs_registry_entry
#' @docType class
NULL

#' Signature
#'
#' @description
#' Represent a signature used by \pkg{rcaiman} to
#' identify the instruments that captured an image and their configuration
#' state at acquisition time.
#'
#' @section Definition:
#' A `signature` declares the expected metadata values used to recognize a
#' configuration state of an imaging system. Its rules are matched against
#' embedded image metadata to identify the applicable specifications.
#'
#' @section Domain of application:
#' A signature acquires full meaning only within a
#' [hs_registry_entry] via `instrument_metadata`, which identifies the
#' physical acquisition system that can produce
#' such data.
#'
#' @section Structure:
#'
#' A `signature` is an S3 object with the following fields:
#'
#' \describe{
#'   \item{id}{Identifier (snake_case).}
#'   \item{dim}{Integer-like vector of length two specifying image width and height.}
#'   \item{validated_with_libraw}{Character vector of validated `LibRaw` versions.}
#'   \item{validated_with_exiftool}{Character vector of validated `ExifTool` versions.}
#'   \item{metadata_profile}{Identifier of the metadata extraction contract.}
#'   \item{rules}{Named list of expected tag–value pairs.}
#'   \item{date}{Date of signature declaration.}
#'   \item{bands}{Named list of [band_spec] objects.}
#' }
#'
#' @section Generation and validation:
#'
#' A signature is created via [add_signature()].
#'
#' Registration validates structural consistency (identifier format, rule
#' structure), but does not perform metadata matching at creation time.
#'
#' @section Retrieving:
#' A `signature` may be retrieved using [get_signature()].
#'
#' @note
#' Can be used independently to supply arguments to \pkg{rcaiman} core
#' functions. However, its domain of validity remains tied as declared in
#' *Domain of application*.
#'
#' @name signature
#' @docType class
NULL

#' Band specification
#'
#' @description
#' Describe the source bands and their spectral interpretation under an
#' signature.
#'
#' @details
#' A band specification is an S3 named list with:
#' \describe{
#'   \item{id}{Identifier unique within the metadata signature.}
#'   \item{cfa_pattern}{Optional 2 x 2 character matrix of source band identifiers.}
#'   \item{spectral_mapping}{Optional named list of expressions deriving target
#'     bands from source bands.}
#'   \item{date}{Date of specification declaration.}
#'   \item{notes}{Optional free-form documentation.}
#'   \item{contact_information}{Optional contact information.}
#'   \item{geometry}{Named list of [geometry_spec] objects.}
#' }
#'
#' Several geometries can share a band interpretation. Radiometry specifications
#' under each geometry inherit this context through their place in the tree.
#' This hierarchy does not prescribe the processing order or select the bands
#' to which a correction should be applied.
#'
#' Band interpretation is distinct from assigning bands to display channels.
#' Omitting CFA and spectral mapping leaves those properties unspecified.
#'
#' @seealso [add_band_spec()], [get_band_spec()], [geometry_spec]
#' @name band_spec
#' @docType class
NULL

#' Geometry specification
#'
#' @description
#' Describe internal viewing directions under a declared band and metadata
#' context.
#'
#' @details
#' A `geometry_spec` is an S3 named list accepted directly as `geometry` by
#' [rcaiman::set_internal_geometry()]. It describes off-axis angle and internal
#' azimuth; acquisition orientation is supplied separately to
#' [rcaiman::direction_image()].
#'
#' @section Structure:
#' \describe{
#'   \item{id}{Identifier unique within its [band_spec].}
#'   \item{model}{Either `"radial_projection"` or `"anisotropic_projection"`.}
#'   \item{parameters}{Model coefficients following
#'     [rcaiman::set_internal_geometry()].}
#'   \item{center_of_vision}{Snapped optical center in continuous image
#'     coordinates `c(x, y_down)`.}
#'   \item{normalization_radius}{Radius in pixels at the normalization angle.
#'     May be fractional. Required for the radial model.}
#'   \item{normalization_angle}{Off-axis angle in degrees where the radial
#'     polynomial equals one. Required for the radial model.}
#'   \item{max_off_axis_angle}{Radial calculation limit in degrees,
#'     defaulting to 90.}
#'   \item{max_observed_off_axis_angle}{Optional maximum off-axis angle in
#'     degrees represented by observations used for geometric calibration.}
#'   \item{date}{Date of specification declaration.}
#'   \item{notes}{Optional free-form documentation.}
#'   \item{contact_information}{Optional contact information.}
#'   \item{radiometry}{Named list of [radiometry_spec] objects.}
#' }
#'
#' The anisotropic model ignores the normalization fields and calculation
#' limit. Its parameter names are `off_axis_coef`,
#' `sin_internal_azimuth_coef`, `cos_internal_azimuth_coef`, and `atan_scale`.
#' Both models and their equations are documented in
#' [rcaiman::set_internal_geometry()].
#'
#' @section Domain of application:
#' The specification belongs to a [band_spec] under an
#' [signature]. Its coordinates refer to the image grid whose
#' `dim` is declared in that metadata signature. For the anisotropic model,
#' these dimensions also define the semidiagonal used by the model.
#'
#' The maximum observed angle records experimental coverage. It is independent
#' of the normalization reference and the calculation limit, and does not
#' establish a model accuracy guarantee or a mask of measured pixels.
#'
#' @section Generation and validation:
#' [add_geometry_spec()] creates the specification. Geometric validation uses
#' the same contract as [rcaiman::set_internal_geometry()], including radial
#' normalization and monotonicity. It does not require the optical center or
#' the normalization circle to fit inside the image.
#'
#' @section Retrieving:
#' Retrieve with [get_geometry_spec()] and pass the object directly to the
#' core. Radial specifications can also describe destinations for
#' [rcaiman::reproject_points()] and [rcaiman::reproject_image()].
#'
#' @seealso [add_geometry_spec()], [rcaiman::normalize_projection_coef()]
#' @name geometry_spec
#' @docType class
NULL

#' Radiometry specification
#'
#' @description
#' Represent a radiometry specification defining flat-field correction
#' operators used by \pkg{rcaiman}
#' to obtain radiometrically consistent data from imaging systems.
#'
#' @section Definition:
#' A `radiometry_spec` is a formal declaration of radiometric interpretation
#' under a specific geometric interpretation of an imaging system.
#'
#' @section Domain of application:
#' A `radiometry_spec` acquires declared validity only when associated with a
#' [geometry_spec] within a [hs_registry_entry]. Its structural
#' position determines the geometric, band, and metadata context under which the
#' radiometric interpretation applies.
#'
#' @section Structure:
#'
#' A `radiometry_spec` is an S3 object with the following fields:
#'
#' \describe{
#'   \item{id}{Identifier (snake_case).}
#'   \item{model}{Character vector identifying the flat-field model.}
#'   \item{parameters}{Named list of fitted parameters by aperture and band, or `NULL`
#'    for `lang2010`, whose coefficients are provided by the core.}
#'   \item{rcaiman_version}{Version of the core recorded at declaration.}
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
#' The model identifier and its fitted parameters are validated at
#' registration time (see [add_radiometry_spec()]). For the application of
#' fitted spatial surfaces, see the "Radiometric preprocessing" section in
#' [rcaiman::predict_trend_surface()].
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
#' @seealso [calibrate_radiometry()]
#'
#' @name radiometry_spec
#' @docType class
NULL
