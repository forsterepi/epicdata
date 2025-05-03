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
    if (identical(self@consent.final, TRUE)) {
      return("@consent can only be TRUE if @id.var has been specified")
    }
  }

  # Avoid direct change of automatically generated default-related variables
  for (i in seq_along(self@var.list)) {
    current_var <- self@var.list[[i]][["var.name"]]

    # touch.na
    check_result <- validator.metadata.default(self, current_var)
    if (!is.null(check_result)) {
      return(check_result)
    }
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

  # Define keys
  keys.var.name <- c("var.name")
  keys.type <- c("type")
  keys.old.id <- c("old.id","id.old")
  keys.label <- c("label")
  keys.label.eng <- c("label.eng","eng.label")
  keys.to.na <- c("to.na","na.to","to.na.default.group","to.na.default.option",
    "to.na.final")
  keys.touch.na <- c("touch.na","na.touch","touch.na.default.group",
    "touch.na.default.option","touch.na.final")
  keys.na.else <- c("na.else","else.na","na.else.default.group","na.else.final")
  keys.na.rules <- c("na.rules","rules.na","na.rules.default.group",
    "na.rules.final")
  keys.na.switch <- c("na.switch","switch.na","na.switch.default.group",
    "na.switch.final")
  keys.na.keep <- c("na.keep","keep.na","na.keep.default.group","na.keep.final")
  keys.na.replace <- c("na.replace","replace.na","na.replace.default.group",
    "na.replace.final")
  keys.group <- c("group")
  keys.new <- c("new")
  keys.ops <- c("ops")
  keys.dict <- c("dict","dict.default.group","dict.final")
  keys.cats <- c("cats","cats.default.group","cats.final")
  keys.cats.eng <- c("cats.eng","eng.cats","cats.eng.default.group",
    "cats.eng.final")
  keys.to.factor <- c("to.factor","to.factor.default.group",
    "to.factor.default.option","to.factor.final")
  keys.factor.name <- c("factor.name","name.factor")
  keys.from <- c("from","from.default.group","from.final")
  keys.from.exclude <- c("from.exclude","from.ex","from.exclude.default.group",
    "from.exclude.final")
  keys.to <- c("to","to.default.group","to.final")
  keys.to.exclude <- c("to.exclude","to.ex","to.exclude.default.group",
    "to.exclude.final")
  keys.date.format <- c("date.format","format.date","format",
    "date.format.default.group","date.format.default.option",
    "date.format.final")
  keys.time.format <- c("time.format","format.time","format",
    "time.format.default.group","time.format.default.option",
    "time.format.final")
  keys.datetime.format <- c("datetime.format","format.datetime","format",
    "datetime.format.default.group","datetime.format.default.option",
    "datetime.format.final")
  key.list <- c(keys.var.name, keys.type, keys.old.id, keys.label,
    keys.label.eng, keys.to.na, keys.touch.na, keys.na.else,
    keys.na.rules, keys.na.switch, keys.na.keep, keys.na.replace,
    keys.group, keys.new, keys.ops, keys.dict, keys.cats, keys.cats.eng,
    keys.to.factor, keys.factor.name, keys.from, keys.from.exclude,
    keys.to, keys.to.exclude, keys.date.format, keys.time.format,
    keys.datetime.format)

  # Loop over all variables
  for (i in seq_along(value)) {
    if (!checkmate::test_subset(names(value[[i]]), empty.ok = FALSE,
          choices = key.list)) {
      invalid <- names(value[[i]]) %>%
        magrittr::extract(names(value[[i]]) %>%
          magrittr::is_in(key.list) %>%
          magrittr::not()) %>% stringi::stri_c(collapse = ", ")
      return(paste0("contains invalid keys in variable '",names(value)[i],
                    "', namely: ",invalid))
    }

    if (!checkmate::test_logical(value[[i]][["touch.na"]], len = 1,
                                 any.missing = FALSE, null.ok = TRUE)) {
      return(paste0("has touch.na (or na.touch) keys in the wrong format in variable '",
                    names(value)[i],"'"))
    }
    if (!checkmate::test_logical(value[[i]][["na.touch"]], len = 1,
                                 any.missing = FALSE, null.ok = TRUE)) {
      return(paste0("has touch.na (or na.touch) keys in the wrong format in variable '",
                    names(value)[i],"'"))
    }
  }

  # Check that var.names are syntactically valid names
  # if (make.names(names(value)) %>% magrittr::equals(names(value)) %>% all()) {
  #   return("Variables must have syntactically valid names")
  # }

}

var.groups.validator <- function(value) {
  test.mode("var.groups.validator")

  # Check basic list structure
  if (!checkmate::test_list(value, types = "list", any.missing = FALSE,
                            min.len = 1, names = "unique", null.ok = TRUE)) {
    return("must be a list of lists, must have unique group names, must have
    at least 1 variable, and must not contain any NAs.")
  }

  # Define keys
  keys.group.name <- c("group.name")
  keys.group.label <- c("group.label")
  keys.group.label.eng <- c("group.label.eng","eng.group.label")
  keys.mc.exclusive <- c("mc.exclusive","mc.ex")
  keys.to.na <- c("to.na","na.to")
  keys.touch.na <- c("touch.na","na.touch")
  keys.na.else <- c("na.else","else.na")
  keys.na.rules <- c("na.rules","rules.na")
  keys.na.switch <- c("na.switch","switch.na")
  keys.na.keep <- c("na.keep","keep.na")
  keys.na.replace <- c("na.replace","replace.na")
  keys.dict <- c("dict","dict.eng")
  keys.dict.rules <- c("dict.rules","rules.dict")
  keys.cats <- c("cats")
  keys.cats.eng <- c("cats.eng","eng.cats")
  keys.cats.rules <- c("cats.rules","rules.cats")
  keys.to.factor <- c("to.factor")
  keys.from <- c("from")
  keys.from.exclude <- c("from.exclude","from.ex")
  keys.to <- c("to")
  keys.to.exclude <- c("to.exclude","to.ex")
  keys.date.format <- c("date.format","format.date")
  keys.time.format <- c("time.format","format.time")
  keys.datetime.format <- c("datetime.format","format.datetime")
  keys.limit.rules <- c("limit.rules","limits.rules","rule.limits",
                        "rules.limits")
  key.list <- c(keys.group.name, keys.group.label, keys.group.label.eng,
    keys.mc.exclusive,
    keys.to.na, keys.touch.na, keys.na.else, keys.na.rules,
    keys.na.switch, keys.na.keep, keys.na.replace, keys.dict, keys.dict.rules,
    keys.cats, keys.cats.eng, keys.cats.rules, keys.to.factor, keys.from,
    keys.from.exclude, keys.to, keys.to.exclude, keys.date.format,
    keys.time.format, keys.datetime.format, keys.limit.rules)

  # Loop over all variables
  for (i in seq_along(value)) {
    if (!checkmate::test_subset(names(value[[i]]), empty.ok = FALSE,
          choices = key.list)) {
      invalid <- names(value[[i]]) %>%
        magrittr::extract(names(value[[i]]) %>%
          magrittr::is_in(key.list) %>%
          magrittr::not()) %>% stringi::stri_c(collapse = ", ")
      return(paste0("contains invalid keys in group '",names(value)[i],
                    "', namely: ",invalid))
    }

    if (!checkmate::test_logical(value[[i]][["touch.na"]], len = 1,
                                 any.missing = FALSE, null.ok = TRUE)) {
      return(paste0("has touch.na (or na.touch) keys in the wrong format in group '",
      names(value)[i],"'"))
    }
    if (!checkmate::test_logical(value[[i]][["na.touch"]], len = 1,
                                 any.missing = FALSE, null.ok = TRUE)) {
      return(paste0("has touch.na (or na.touch) keys in the wrong format in group '",
                    names(value)[i],"'"))
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
validator.metadata.default <- function(self, current_var) {
  test.mode(paste0("validate.default.consistency.",current_var))

  # touch.na
  ## .default.option
  if (!identical(self@var.list[[current_var]][["touch.na.default.option"]],
                 self@touch.na)) {
    return(paste0("Do not change @var.list$",current_var,
                  "$touch.na.default.option directly. Change @touch.na instead."))
  }
  ## .default.group
  if (is.null(self@var.list[[current_var]][["group"]])) {
    if (!is.null(self@var.list[[current_var]][["touch.na.default.group"]])) {
      return(paste0("Do not change @var.list$",current_var,
            "$touch.na.default.group directly. Change @var.groups instead."))
    }
  } else {
    current_group <- self@var.list[[current_var]][["group"]]

    if (!identical(self@var.list[[current_var]][["touch.na.default.group"]],
                   self@var.groups[[current_group]][["touch.na"]])) {
      return(paste0("Do not change @var.list$",current_var,
                    "$touch.na.default.group directly. Change @var.groups instead."))
    }
  }

  # ADD MORE DEFAULTS HERE
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
