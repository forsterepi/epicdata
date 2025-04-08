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
    id.pattern = meta.prop.id.pattern,
    touch.na = meta.prop.touch.na,
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
