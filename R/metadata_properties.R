#' @include metadata_validator.R
#' @include metadata_process_input.R
NULL


# Options -----------------------------------------------------------------

meta.prop.study.name <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    test.mode("option.study.name.validator")

    if (!checkmate::test_character(value, len = 1, min.chars = 1,
                                  any.missing = FALSE, null.ok = TRUE)) {
      return("must have length 1 and must not be empty or NA")
    }
  },
  setter = function(self, value) {
    test.mode("option.study.name.setter")

    self@study.name <- setter.select.default(option = value, pre = "my_study")
    self
  }
)

meta.prop.id.var <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    test.mode("option.id.var.validator")

    if (!checkmate::test_character(value, len = 1, min.chars = 1,
                                  any.missing = FALSE, null.ok = TRUE)) {
      return("must have length 1 and must not be empty or NA")
    }
    if (!is.null(value)) {
      if (!identical(make.names(value), value)) {
        return("must be a syntactically valid name")
      }
    }
  }
)

meta.prop.id.pattern <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    test.mode("option.id.pattern.validator")

    if (!checkmate::test_character(value, len = 1, min.chars = 1,
                                  any.missing = FALSE, null.ok = TRUE)) {
      return("must have length 1 and must not be empty or NA")
    }
  }
  # add to validator a check for valid regex (test the value in the function that is actually used in the workflow)
)

meta.prop.consent <- S7::new_property(
  class = NULL | S7::class_logical,
  validator = function(value) {
    test.mode("option.consent.validator")

    if (!checkmate::test_logical(value, len = 1, any.missing = FALSE,
                                 null.ok = TRUE)) {
      return("must have length 1 and must not be NA")
    }
  }
)

meta.prop.consent.final <- S7::new_property(
  getter = function(self) {
    test.mode("consent.final.getter")

    if (is.null(self@id.var)) {
      setter.select.default(option = self@consent, pre = FALSE)
    } else {
      setter.select.default(option = self@consent, pre = TRUE)
    }
  }
)

meta.prop.touch.na <- S7::new_property(
  class = NULL | S7::class_logical,
  validator = function(value) {
    test.mode("option.touch.na.validator")

    if (!checkmate::test_logical(value, len = 1, any.missing = FALSE,
                                null.ok = TRUE)) {
      return("must have length 1 and must not be NA")
    }
  },
  setter = function(self, value) {
    test.mode("option.touch.na.setter")

    # Update touch.na (first, so that validator runs before changing var.list or na.touch)
    self@touch.na <- value

    # Update var.list
    updated.var.list <- self@var.list
    for (i in seq_along(updated.var.list)) {
      updated.var.list[[i]]$touch.na.default.option <- value
    }
    self@var.list <- updated.var.list

    # Update na.touch, if different from touch.na
    if (!identical(self@na.touch, value)) {
      self@na.touch <- value
    }

    # Return self
    self
  }
)

meta.prop.na.touch <- S7::new_property(
  class = NULL | S7::class_logical,
  validator = function(value) {
    test.mode("option.na.touch.validator")

    if (!checkmate::test_logical(value, len = 1, any.missing = FALSE,
                                 null.ok = TRUE)) {
      return("must have length 1 and must not be NA")
    }
  },
  setter = function(self, value) {
    test.mode("option.na.touch.setter")

    # Update na.touch
    self@na.touch <- value

    # Update touch.na, if different from na.touch
    if (!identical(self@touch.na, value)) {
      self@touch.na <- value
    }

    # Return self
    self
  }
)


# var.list ----------------------------------------------------------------

meta.prop.var.list <- S7::new_property(
  class = S7::class_list, # var.list cannot be NULL
  validator = var.list.validator,
  setter = function(self, value) {
    test.mode("var.list.setter")

    # Check for newly created variables
    if (!is.null(self@var.list)) {
      if (length(value) > length(self@var.list)) {
        cli::cli_abort(c("Error in creating new variables",
          "i" = "Please create new variables via the YAML file."),
          call = rlang::expr(metadata@var.list <- value),
          class = "error.meta.prop.var.list.1")
      }
    }

    # Update names
    for (i in seq_along(value)) {
      # Make element group.name and the name of the list element consistent
      rlang::try_fetch({names(value)[i] <- value[[i]]$var.name},
        error = function(cnd) {
          cli::cli_abort(c("Key {.var var.name} cannot be set to {.var NULL}",
            "i" = "To delete the variable, set @var.list${names(value)[i]}
            to {.var NULL}."),
          call = craft.call.var.list(variable = names(value)[i],
                                     key = "var.name", value = NULL),
          class = "error.meta.prop.var.list.2")
        }
      )
    }

    #Pre-validate (but if we pre-validate, processed inputs like cats, dict
    # must be validated in the corresponding processing function)
    check_result <- var.list.validator(value)
    if (!is.null(check_result)) {
      cli::cli_abort(paste0("@var.list ",check_result),
        call = rlang::caller_env(), class = "error.meta.prop.var.list.3")
      ## SOLVE CALLER_ENV PROBLEMS
    }

    # Process inputs
    value %<>% setter.variable.process.inputs(self)
    ## Update input validation with checkmate in setter.variable.cats()

    # Alternative names
    value %<>% setter.variable.update.alternative.names(self)

    # Update default.group elements when changing group
    value %<>% setter.variable.update.default.group(self)

    # Create .final keys
    value %<>% setter.variable.create.final()

    # Set the new value
    self@var.list <- value
    self
  }
)

meta.prop.var.names <- S7::new_property(
  getter = function(self) {
    test.mode("var.names.getter")

    names(self@var.list)
  }
)

# var.groups --------------------------------------------------------------

meta.prop.var.groups <- S7::new_property(
  class = NULL | S7::class_list,
  validator = var.groups.validator,
  setter = function(self, value) {
    test.mode("var.groups.setter")

    # If the last element is removed, remove the empty list as well
    if (is.list(value) & length(value) == 0) {
      value <- NULL
    }

    ##WHAT IF INVALID VALUE FOR GROUP.NAME???, e.g. NA

    # Update names
    for (i in seq_along(value)) {
      # Make element group.name and the name of the list element consistent
      rlang::try_fetch({names(value)[i] <- value[[i]]$group.name},
        error = function(cnd) {
          cli::cli_abort(c("Error in creating/deleting {.var var.group}.",
            "i" = "To delete a group, set @var.groups${names(value)[i]}
            <- NULL.",
            "i" = 'To create a new group, set @var.groups${names(value)[i]}$group.name
            <- "{names(value)[i]}"'),
          call = rlang::caller_env(), class = "error.meta.prop.var.groups.1")
          ## SOLVE CALLER_ENV PROBLEMS
        }
      )
    }

    #Pre-validate
    check_result <- var.groups.validator(value)
    if (!is.null(check_result)) {
      cli::cli_abort(paste0("@var.groups ",check_result),
        call = rlang::caller_env(), class = "error.meta.prop.var.groups.2")
      ## SOLVE CALLER_ENV PROBLEMS
    }

    # Alternative names
    value %<>% setter.group.update.alternative.names(self)

    # Update .default.group keys in var.list
    if (is.null(value)) {
      for (i in seq_along(self@var.list)) {
        self@var.list[[i]]$touch.na.default.group <- NULL

        # ADD MORE DEFAULTS HERE
      }
    } else {
      updated.var.list <- setter.group.update.var.list(self, value)
      self@var.list <- updated.var.list
    }

    # Set the new values
    self@var.groups <- value
    self
  }
)

meta.prop.group.names <- S7::new_property(
  getter = function(self) {
    test.mode("group.names.getter")

    names(self@var.groups)
  }
)


# Modules -----------------------------------------------------------------

meta.prop.DUP_NO.ID <- S7::new_property(
  getter = function(self) {
    test.mode("DUP_NO.ID.getter")

    if (is.null(self@id.var)) {
      TRUE
    } else {
      FALSE
    }
  }
)

meta.prop.DUP_FREQ <- S7::new_property(
  getter = function(self) {
    test.mode("DUP_FREQ.getter")

    if (is.null(self@id.var)) {
      FALSE
    } else {
      TRUE
    }
  }
)


# Setter ------------------------------------------------------------------

#' Process inputs in `@var.list` setter function
#'
#' Sometimes, the input required in YAML does not correspond to the object
#' saved in S7 class `epicdata::metadata`. This function summarizes processing
#' efforts in the `@var.list` setter function.
#'
#' @param value The `value` argument of the setter function, i.e., the new value
#' for `@var.list`.
#'
#' @returns An updated version of input `value`.
#'
#' @noRd
setter.variable.process.inputs <- function(value, self) {
  # Get var.names
  var.names <- names(value)

  # Get new.order
  new.order <- rep(NA, length(value))
  for (i in seq_along(value)) {
    if (!is.null(value[[i]]$new)) {
      new.order[i] <- value[[i]]$var.name
    }
  }
  if (new.order %>% is.na() %>% all()) {
    new.order <- character(0)
  } else {
    new.order <- new.order[!is.na(new.order)]
  }

# Process
  for (i in seq_along(value)) {
    current_var <- names(value)[i]

    # Process cats input
    if (!identical(value[[current_var]]$cats, self@var.list[[current_var]]$cats)) {
      value[[i]]$cats <- process.cats(cats = value[[i]]$cats,
                                              name = value[[i]]$var.name,
                                              eng = FALSE)
    }
    # Process cats.eng input
    if (!identical(value[[current_var]]$cats.eng, self@var.list[[current_var]]$cats.eng)) {
      value[[i]]$cats.eng <- process.cats(cats = value[[i]]$cats.eng,
                                                  name = value[[i]]$var.name,
                                                  eng = TRUE)
    }
    # Process new input
    if (!identical(value[[current_var]]$new, self@var.list[[current_var]]$new)) {
      value[[i]]$new <- process.new(new = value[[i]]$new,
                                    var = value[[i]]$var.name,
                                    var.names = var.names,
                                    new.order = new.order)
    }
  }

  # Return
  value
}

#' Title
#'
#' Desc.
#'
#' @param x Desc.
#'
#' @noRd
setter.variable.update.alternative.names <- function(value, self) {
  for (i in seq_along(value)) {
    # Get var.name
    current_var <- value[[i]][["var.name"]]

    # touch.na / na.touch
    if (!identical(self@var.list[[current_var]][["touch.na"]],
                   value[[current_var]][["touch.na"]])) {
      value[[i]][["na.touch"]] <- value[[i]][["touch.na"]]
    }
    if (!identical(self@var.list[[current_var]][["na.touch"]],
                   value[[current_var]][["na.touch"]])) {
      value[[i]][["touch.na"]] <- value[[i]][["na.touch"]]
    }

  }

  # Return
  value
}

#' Title
#'
#' Desc.
#'
#' @param x Desc.
#'
#' @noRd
setter.group.update.alternative.names <- function(value, self) {
  for (i in seq_along(value)) {
    # Get var.name
    current_group <- value[[i]][["group.name"]]

    # touch.na / na.touch
    if (!identical(self@var.groups[[current_group]][["touch.na"]],
                  value[[current_group]][["touch.na"]])) {
      value[[i]][["na.touch"]] <- value[[i]][["touch.na"]]
    }
    if (!identical(self@var.groups[[current_group]][["na.touch"]],
                  value[[current_group]][["na.touch"]])) {
      value[[i]][["touch.na"]] <- value[[i]][["na.touch"]]
    }

  }

  # Return
  value
}

#' Update .default.group keys when changing the group key in `@var.list`
#'
#' Usually, the .default.group keys change when the corresponding values in
#' `@var.groups` change. Hoewever, when the group key of variables change, the
#' .default.group values need to updated as well.
#'
#' @param value The `value` argument of the setter function, i.e., the new value
#' for `@var.list`.
#'
#' @returns An updated version of input `value`.
#'
#' @noRd
setter.variable.update.default.group <- function(value, self) {
  # Get group names
  # Run outside the loop so that the @group.names getter function only runs once
  group_names <- self@group.names

  # Loop over all variables in the updated var.list, i.e., value
  for (i in seq_along(value)) {
    # Get current variable
    current_var <- value[[i]][["var.name"]]

    # Only make adjustments if the value for group actually changed
    if (!identical(self@var.list[[current_var]][["group"]],
                   value[[current_var]][["group"]])) {
      # If group is now NULL, turn all .default.group elements to NULL as well
      if (is.null(value[[i]][["group"]])) {
        value[[i]][["touch.na.default.group"]] <- NULL
        # ADD MORE DEFAULTS HERE
      } else {
        # Check if the new group actually exists in var.groups
        if (value[[i]][["group"]] %in% group_names) {
          # If the new group exists, search for the correct group
          for (j in seq_along(self@var.groups)) {
            if (identical(self@var.groups[[j]][["group.name"]], value[[i]][["group"]])) {
              # Update the .default.group value
              value[[i]][["touch.na.default.group"]] <- self@var.groups[[j]][["touch.na"]]
              # ADD MORE DEFAULTS HERE
            }
          }
        } else {
          # If the new group does not exist, turn .default.group back to NULL
          value[[i]][["touch.na.default.group"]] <- NULL
          # ADD MORE DEFAULTS HERE
        }
      }
    }
  }

  # Return
  value
}

#' Create .final keys in `@var.list`
#'
#' After collecting all information from global and group defaults, it needs to
#' be summarized to the actual assigned values, namely the .final keys.
#'
#' @param value The `value` argument of the setter function, i.e., the new value
#' for `@var.list`.
#'
#' @returns An updated version of input `value`.
#'
#' @noRd
setter.variable.create.final <- function(value) {
  # Update values
  for (i in seq_along(value)) {
    # touch.na
    value[[i]][["touch.na.final"]] <- setter.select.default(
      var = value[[i]][["touch.na"]],
      group = value[[i]][["touch.na.default.group"]],
      option = value[[i]][["touch.na.default.option"]],
      pre = TRUE
    )
    # ADD MORE DEFAULTS HERE

    # # to.factor: if factor.name has been specified, to.factor.final is overwritten to TRUE
    # if (is.null(value[[i]][["factor.name"]])) {
    #   value[[i]][["to.factor.final"]] <- setter.select.default(
    #     var = value[[i]][["to.factor"]],
    #     group = value[[i]][["to.factor.default.group"]],
    #     option = value[[i]][["to.factor.default.option"]],
    #     pre = FALSE
    #   )
    # } else {
    #   # Version 1
    #   value[[i]][["to.factor.final"]] <- setter.select.default(
    #     var = TRUE,
    #     group = value[[i]][["to.factor.default.group"]],
    #     option = value[[i]][["to.factor.default.option"]],
    #     pre = FALSE
    #   )
    #   # Version 2
    #   value[[i]][["to.factor.final"]] <- TRUE
    # }

  }

  # Return
  value
}

#' Update .default.group keys in `@var.list`
#'
#' When changing values in `@var.groups`, the corresponding .default.group keys
#' in `@var.list` need to be updated.
#'
#' @param self The `self` argument of the setter function, i.e., the complete
#' metadata object before updating.
#' @param value The `value` argument of the setter function, i.e., the new value
#' for `@var.groups`.
#'
#' @returns An updated version of `@var.list`.
#'
#' @noRd
setter.group.update.var.list <- function(self, value) {
  # Check if groups were added or deleted (this should include name changes)
  ## Using names() below only works if this function is used after making
  ## element group.name and the name of the list element consistent
  old_names <- names(self@var.groups)
  new_names <- names(value)

  group_names <- c(old_names, new_names) %>% unique()

  # Get copy of var.list to update
  updated.var.list <- self@var.list

  # Loop over all .default.group variables
  for (i in seq_along(group_names)) {
    # Get current group name
    current_group <- group_names[i]
    # Loop over all variables
    for (j in seq_along(updated.var.list)) {
      # Check if the specified group is the current group
      if (identical(updated.var.list[[j]][["group"]], current_group)) {
        # Update .default.group values
        updated.var.list[[j]][["touch.na.default.group"]] <- value[[current_group]][["touch.na"]]
        # ADD MORE DEFAULTS HERE
      }
    }
  }

  # Return
  updated.var.list
}

#' Select the correct value from the default cascade
#'
#' @param var Value provided in `@var.list`.
#' @param group Value provided in `@var.groups`.
#' @param option Value provided in the corresponding option.
#' @param pre Hard-coded pre-defined value.
#'
#' @returns One of the 4 provided values, based on which ones are available.
#'
#' @noRd
setter.select.default <- function(var = NULL, group = NULL, option = NULL,
                                  pre = NULL) {
  if (!is.null(var)) {
    out <- var
  } else {
    if (!is.null(group)) {
      out <- group
    } else {
      if (!is.null(option)) {
        out <- option
      } else {
        out <- pre
      }
    }
  }

  out
}



# viol <- y$var.list$id$dict.viol
#
# lobstr::ast(!!rlang::parse_expr(viol[3]))
# rlang::expr(mutate(df, type := case_when(height > 200 | mass > 200 ~ "large", species == "Droid" ~ "robot", .default = type))) %>% View()



# variable property remains

# var.prop.name <- S7::new_property(
#   class = S7::class_character,
#   default = quote(stop("@name is required")),
#   validator = function(value) {
#     if (length(value) != 1L) {
#       "must be length 1"
#     } else if (make.names(value) != value) {
#       "must be a syntactically valid name"
#     }
#   }
# )
#
# var.prop.type <- S7::new_property(
#   class = S7::class_character,
#   default = quote(stop("@type is required")),
#   validator = function(value) {
#     if (length(value) != 1L) {
#       "must be length 1"
#     } else if (!(value %in% c("text","cat","num","date","time","datetime"))) {
#       "must be one of 'text','cat','num','date','time','datetime'"
#     }
#   }
# )
#
# var.prop.old.id <- S7::new_property(
#   class = NULL | S7::class_character,
#   validator = function(value) {
#     if (!(is.null(value) | length(value) == 1L)) {
#       "must have length 1"
#     }
#   }
# )
#
# var.prop.label <- S7::new_property(
#   class = NULL | S7::class_character,
#   validator = function(value) {
#     if (!(is.null(value) | length(value) == 1L)) {
#       "must have length 1"
#     }
#   }
# )
#
# var.prop.touch.na <- S7::new_property(
#   class = NULL | S7::class_logical,
#   validator = function(value) {
#     if (!(is.null(value) | length(value) == 1L)) {
#       "must have length 1"
#     }
#   }
# )
