#' @include metadata_constructor.R
#' @include metadata_validator.R
#' @include metadata_properties.R
NULL

#' Metadata objects
#'
#' Constructor function for S3 class `epicdata_meta`.
#'
#' @details `epicdata_meta` objects are lists containing 5 data.frames.
#'   These data.frames are described below:
#'
#'   `options` contains
#'
#'   `var.list` contains
#'
#'   `na.codes` contains
#'
#'   `na.rules` contains
#'
#'   `contras` contains
#'
#' @param file Path to yaml
#'
#' @returns A `epicdata_meta` object. Its structure is described in
#'   Details.
#'
#' @export
#'
metadata <- S7::new_class("metadata",
  properties = list(
    id.var = prop.id.var,
    vars = S7::class_character
  ),
  validator = metadata_validator,
  constructor = metadata_constructor
)
