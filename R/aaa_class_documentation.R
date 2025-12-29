#' Instrument registry
#'
#' @description
#' Objects of class `hemispherical_camera_registry` represent instrument
#' specifications and calibration records used by the rcaiman ecosystem. They
#' are intended to store instrument metadata and one or more calibrations,
#' together with soft metadata describing provenance.
#'
#' @details
#' This is an S3 class implemented as a named list. Its purpose is to make
#' explicit, inspectable, and auditable the geometric and radiometric
#' assumptions applied during canopy image preprocessing.
#'
#' The registry may contain multiple calibrations for the same instrument.
#' Metadata is intended to provide the information needed to decide which to
#' choose, or a reference to where to find such information.
#'
#' @section Minimum operational contract:
#' A `hemispherical_camera_registry` object must provide enough information to:
#' \itemize{
#'   \item map pixel radius to zenith angle,
#'   \item identify the optical center,
#'   \item define the usable image radius,
#'   \item correct radiometric attenuation (vignetting).
#' }
#'
#' @section Structure:
#' The object is a named list with at least two elements:
#' \describe{
#'   \item{instrument_metadata}{List describing the physical instrument and its provenance.}
#'   \item{calibrations}{Named list of calibration records, each with metadata,
#'   geometry specifications and associated optional radiometry specifications.}
#' }
#'
#' @seealso [new_registry()]
#'
#' @name hemispherical_camera_registry
#' @docType class
NULL
