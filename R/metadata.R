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
    study.name = meta.prop.study.name,
    id.var = meta.prop.id.var,
    # variables
    vars = S7::class_character,
    # workflow modules
    DUP_NO.ID = meta.prop.DUP_NO.ID,
    DUP_FREQ = meta.prop.DUP_FREQ
  ),
  validator = metadata_validator,
  constructor = metadata_constructor
)
