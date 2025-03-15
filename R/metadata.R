#' @include metadata_constructor.R
#' @include metadata_validator.R
#' @include metadata_properties.R
NULL

#' Metadata objects
#'
#' S7 class `epicmodel::metadata`
#'
#' @param file Path to yaml file
#'
#' @returns An S7 `epicdata::metadata` object
#'
#' @export
#'
metadata <- S7::new_class("metadata",
  properties = list(
    # options
    study.name = prop.study.name,
    id.var = prop.id.var,
    # variables
    vars = S7::class_character
    # workflow modules

  ),
  validator = metadata_validator,
  constructor = metadata_constructor
)
