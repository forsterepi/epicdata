#' @include metadata_constructor.R
#' @include metadata_validator.R
#' @include metadata_properties.R
NULL

# S7 class ----------------------------------------------------------------

#' Metadata objects
#'
#' S7 class `epicdata::metadata`
#'
#' @param file Path to yaml file
#'
#' @prop data.name The name of the data.frame. A single string.
#' @prop id.var The name of the ID variable. A variable name used in `var.list`.
#'
#' @returns A S7 `epicdata::metadata` object
#'
#' @export
#'
metadata <- S7::new_class(
  "metadata",
  properties = list(
    # options
    data.name = meta.prop.data.name,
    id.var = meta.prop.id.var,
    id.pattern = meta.prop.id.pattern,
    consent = meta.prop.consent,
    consent.final = meta.prop.consent.final,
    remove.vars = meta.prop.remove.vars,
    vars.remove = meta.prop.vars.remove,
    touch.na = meta.prop.touch.na,
    na.touch = meta.prop.na.touch, # alternative name
    # variables
    var.list = meta.prop.var.list,
    var.names = meta.prop.var.names,
    # var.str = meta.prop.var.str, # ??
    # groups
    var.groups = meta.prop.var.groups,
    group.names = meta.prop.group.names,
    # workflow modules
    VAR_DF.NOT.META_NOTE = meta.prop.VAR_DF.NOT.META_NOTE,
    VAR_DF.NOT.META_RM = meta.prop.VAR_DF.NOT.META_RM,
    DUP_NO.ID = meta.prop.DUP_NO.ID,
    DUP_FREQ = meta.prop.DUP_FREQ
  ),
  validator = metadata.validator,
  constructor = metadata.constructor
)


# Extractors --------------------------------------------------------------

#' Extract the names of all variables of a certain type
#'
#' @param metadata A S7 `epicdata::metadata` object
#' @param type A single string, one of `text`, `cat`, `num`, `date`, `time`,
#' `datetime`
#'
#' @returns A character string with the names of all variables of the provided
#' type in the order of appearance in the metadata.
#' @export
#'
#' @examples
metadata.all.vars.of.type <- function(metadata, type) {
  # Check inputs
  S7::check_is_S7(metadata)
  checkmate::assert_class(metadata, "epicdata::metadata", null.ok = FALSE)

  checkmate::assert_string(type, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_choice(
    type,
    choices = c("text", "num", "cat", "date", "time", "datetime")
  )

  # Extract variable names
  metadata@var.list %>%
    purrr::map_lgl(
      \(x) x$type == type
    ) %>%
    names(.)[.]
}
