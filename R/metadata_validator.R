metadata.validator <- function(self) {
  test.mode("metadata.validator")

  var_names <- self@var.names
  group_names <- self@group.names

  # option: id.var
  if (!is.null(self@id.var)) {
    if (!(self@id.var %in% var_names)) {
      return("@id.var must be a variable specified in var.list")
    }
  }

  # option: consent
  if (is.null(self@id.var)) {
    if (self@consent.final == TRUE) {
      return("@consent can only be TRUE if @id.var has been specified")
    }
  }

  # Avoid direct change of automatically generated default-related variables
  for (i in seq_along(self@var.list)) {
    current_var <- self@var.list[[i]][["var.name"]]

    # touch.na
    check_result <- validator.metadata.default.touch.na(self, current_var)
    if (!is.null(check_result)) {
      return(check_result)
    }

    # ADD MORE DEFAULTS HERE
  }
}

var.list.validator <- function(value) {
  test.mode("var.list.validator")

  # Check basic list structure
  if (!checkmate::test_list(value, types = "list", any.missing = FALSE,
                            min.len = 1, names = "unique", null.ok = FALSE)) {
    return("must be a list of lists, must have unique variable names, must have
    at least 1 variable, must not contain any NAs, and must not be NULL.")
  }
  # Loop over all variables
  for (i in seq_along(value)) {
    if (!checkmate::test_subset(names(value[[i]]), empty.ok = FALSE,
                     choices = c("var.name","type","old.id","label",
                     "label.eng","to.na","touch.na","na.touch","na.else","na.rules",
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

    if (!checkmate::test_logical(value[[i]][["touch.na"]], len = 1,
                                 any.missing = FALSE, null.ok = TRUE)) {
      return("has touch.na keys in the wrong format")
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

  for (i in seq_along(value)) {
    if (!checkmate::test_logical(value[[i]][["touch.na"]], len = 1,
                                 any.missing = FALSE, null.ok = TRUE)) {
      return("has touch.na keys in the wrong format")
    }
  }
}

#' Prevent direct change of internal default keys for `touch.na`
#'
#' `@var.list` contains internal key for default handling, namely
#' .default.option, .default.group, and .final. They should not be changed
#' directly by the user, but only indirectly, when the corresponding key in
#' `@var.list`, the corresponding value in `@var.groups` or the corresponding
#' option change. This function ensures this behavior for `touch.na` be
#' validating the consistency of the values.
#'
#' @param self The `self` argument of the `metadata.validator()` function, i.e.,
#' the updated metadata object to be validated.
#' @param current_var This function is repeated for every variable. `current_var`
#' is the name of the variable in `@var.list` currently validated.
#'
#' @returns A character describing the error if an error actually occurred or
#' NULL otherwise.
#'
#' @noRd
validator.metadata.default.touch.na <- function(self, current_var) {
  test.mode(paste0("validate.default.consistency.",current_var))

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

  # Check consistency of var.group$touch.na and $touch.na.default.group
  if (is.null(self@var.list[[current_var]][["group"]])) {
    if (!is.null(self@var.list[[current_var]][["touch.na.default.group"]])) {
      return(paste0("Do not change @var.list$",current_var,
            "$touch.na.default.group directly. Change @var.groups instead."))
    }
  } else {
    current_group <- self@var.list[[current_var]][["group"]]

    if (is.null(self@var.groups[[current_group]][["touch.na"]])) {
      if (!is.null(self@var.list[[current_var]][["touch.na.default.group"]])) {
        return(paste0("Do not change @var.list$",current_var,
              "$touch.na.default.group directly. Change @var.groups instead."))
      }
    } else {
      if (is.null(self@var.list[[current_var]][["touch.na.default.group"]])) {
        return(paste0("Do not change @var.list$",current_var,
              "$touch.na.default.group directly. Change @var.groups instead."))
      }
      if (self@var.list[[current_var]][["touch.na.default.group"]] != self@var.groups[[current_group]][["touch.na"]]) {
          return(paste0("Do not change @var.list$",current_var,
                "$touch.na.default.group directly. Change @var.groups instead."))
      }
    }
  }
}


# variable validator remains

# validator = function(self) {
#   # old.id
#   if (!is.null(self@old.id)) {
#     if (self@name == self@old.id) {
#       "@old.id must not be the same as @name"
#     }
#     # label & label.eng
#   } else if (!is.null(self@label.eng)) {
#     if (is.null(self@label)) {
#       "@label.eng can only be used together with @label"
#     }
#     # cats & cats.eng
#   } else if (self@type != "cat" & !is.null(self@cats)) {
#     "@cats can only be used for variables of @type 'cat'"
#   } else if (self@type != "cat" & !is.null(self@cats.eng)) {
#     "@cats.eng can only be used for variables of @type 'cat'"
#   } else if (!is.null(self@cats.eng)) {
#     if (is.null(self@cats)) {
#       "@cats.eng can only be used together with @cats"
#     } else if (length(self@cats$level) != length(self@cats.eng$level)) {
#       "@cats.eng must have levels identical to @cats"
#     } else if (!all(self@cats$level == self@cats.eng$level)) {
#       "@cats.eng must have levels identical to @cats"
#     }
#   }
# }
