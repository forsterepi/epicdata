metadata.validator <- function(self) {
  test.mode("metadata.validator")

  var_names <- self@var.names
  grou_names <- self@group.names

  if (!is.null(self@id.var)) {
    if (!(self@id.var %in% var_names)) {
      return("@id.var must be a variable specified in var.list")
    }
  }

  # Avoid direct change of automatically generated default-related variables
  for (i in seq_along(self@var.list)) {
    current_var <- self@var.list[[i]][["var.name"]]

    # touch.na
    return(validator.metadata.default.touch.na(self, current_var))

    # add checks for .default.group & .final to validator.metadata.default.touch.na

    # ADD MORE DEFAULTS HERE
  }
}



var.list.validator <- function(value) {
  test.mode("var.list.validator")

  # Check basic list structure
  if (!checkmate::test_list(value, types = "list", any.missing = FALSE,
                            min.len = 1, names = "unique", null.ok = FALSE)) {
    return("must be a list of lists, must have unique variable names,
    must have at least 1 variable, must not contain any NAs,
           and must not be NULL.")
  }
  # Loop over all variables
  for (i in seq_along(value)) {
    if (!checkmate::test_subset(names(value[[i]]), empty.ok = FALSE,
                     choices = c("var.name","type","old.id","label",
                     "label.eng","to.na","touch.na","na.else","na.rules",
                     "na.switch","na.keep","na.replace","group","new",
                     "ops","dict","cats","cats.eng","to.factor","factor.name",
                     "name.factor","from","from.exclude","to","to.exclude",
                     "format","date.format","time.format","datetime.format",
                     "to.na.default.group","touch.na.default.group",
                     "na.else.default.group","na.rules.default.group",
                     "na.switch.default.group","na.keep.default.group",
                     "na.replace.default.group","dict.default.group",
                     "cats.default.group","cats.eng.default.group",
                     "to.factor.default.group","from.default.group",
                     "from.exclude.default.group","to.default.group",
                     "to.exclude.default.group","format.default.group",
                     "date.format.default.group","time.format.default.group",
                     "datetime.format.default.group",
                     "to.na.default.option","touch.na.default.option",
                     "to.factor.default.option","date.format.default.option",
                     "time.format.default.option",
                     "datetime.format.default.option",
                     "to.na.final","touch.na.final","na.else.final",
                     "na.rules.final","na.switch.final","na.keep.final",
                     "na.replace.final","dict.final","cats.final",
                     "cats.eng.final","to.factor.final","from.final",
                     "from.exclude.final","to.final","to.exclude.final",
                     "format.final","date.format.final","time.format.final",
                     "datetime.format.final"
                     ))) {
      return("contains invalid keys")
    }
  }

  # Check that var.names are syntactically valid names
  # if (make.names(names(value)) %>% magrittr::equals(names(value)) %>% all()) {
  #   return("Variables must have syntactically valid names")
  # }

}

var.groups.validator <- function(value) {
  test.mode("var.groups.validator")

  # group.names must be unique

  return(NULL) # Remove later
}

validator.metadata.default.touch.na <- function(self, current_var) {
  # Check consistency of self@touch.na and $touch.na.default.option
  if (is.null(self@touch.na)) {
    if (!is.null(self@var.list[[current_var]][["touch.na.default.option"]])) {
      return(paste0("Do not change @var.list$",current_var,
            "$touch.na.default.option directly. Change @touch.na instead."))
    }
  } else {
    if (is.null(self@var.list[[current_var]][["touch.na.default.option"]])) {
      return(paste0("Do not change @var.list$",current_var,
            "$touch.na.default.option directly. Change @touch.na instead."))
    }
    if (self@var.list[[current_var]][["touch.na.default.option"]] != self@touch.na) {
      return(paste0("Do not change @var.list$",current_var,
            "$touch.na.default.option directly. Change @touch.na instead."))
    }
  }
}
