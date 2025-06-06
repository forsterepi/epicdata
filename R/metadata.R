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
    data.name = meta.prop.data.name,
    id.var = meta.prop.id.var,
    id.pattern = meta.prop.id.pattern,
    consent = meta.prop.consent,
    consent.final = meta.prop.consent.final,
    touch.na = meta.prop.touch.na,
    na.touch = meta.prop.na.touch, # alternative name
    # variables
    var.list = meta.prop.var.list,
    var.names = meta.prop.var.names,
    # groups
    var.groups = meta.prop.var.groups,
    group.names = meta.prop.group.names,
    # workflow modules
    DUP_NO.ID = meta.prop.DUP_NO.ID,
    DUP_FREQ = meta.prop.DUP_FREQ
  ),
  validator = metadata.validator,
  constructor = metadata.constructor
)
