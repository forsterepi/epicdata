#' @include metadata_validator.R
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

    if (is.null(value)) {
      self@study.name <- "my_study"
    } else {
      self@study.name <- value
    }
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
      if (make.names(value) != value) {
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

    updated.var.list <- self@var.list
    for (i in seq_along(updated.var.list)) {
      updated.var.list[[i]]$touch.na.default.option <- value
    }
    self@var.list <- updated.var.list

    self@touch.na <- value
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
          call = rlang::caller_env(), class = "error.meta.prop.var.list.1")
        ## SOLVE CALLER_ENV PROBLEMS
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
          call = rlang::caller_env(), class = "error.meta.prop.var.list.2")
      ## SOLVE CALLER_ENV PROBLEMS
        }
      )
    }

    # Process inputs
    value %<>% setter.variable.process.inputs()

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

#' Process `cats` and `cats.eng`
#'
#' Used in function `setter.variable.process.inputs()`
#'
#' @param cats A character vector containing the category specification from the
#' YAML input.
#' @param name A single character. It contains the name of the variable and
#' is used in error messages.
#' @param eng TRUE or FALSE (default). If FALSE, "cats" is used in error
#' messages. IF TRUE, "cats.eng" is used in error messages.
#' @param call The caller environment used in error messages to supply the right
#' call to the user.
#'
#' @returns A data.frame, which contains the processed information of the
#' character input for variable keys `cats` and `cats.eng`.
#'
#' @noRd
setter.variable.cats <- function(cats, name, eng = FALSE,
                                 call = rlang::caller_env()) {
  # Check input: eng
  if (!is.logical(eng) | is.na(eng)) {
    cli::cli_abort(c("x" = "Error IE006"), call = call, .internal = TRUE,
                   class = "IE006")
  }

  # Based on eng, create cats_type, which is used in error messages
  if (eng) {
    cats_type <- "cats.eng"
  } else {
    cats_type <- "cats"
  }

  # If cats = NULL, return NULL
  if (is.null(cats)) {
    return(NULL)
  }

  # cats cannot be NA or contain any NA
  if (is.na(cats) %>% any()) {
    cli::cli_abort(c("!" = "{.var {cats_type}} must not be NA or contain any
                     NA.",
                     "i" = "Applies to variable {.var {name}}."),
                   call = call, class = "error.setter.variable.cats.1")
  }

  # cats must be character
  if (!is.character(cats)) {
    cli::cli_abort(c("!" = "{.var {cats_type}} must be specified as character
                     vector.",
                     "i" = "Applies to variable {.var {name}}.",
                     "i" = "See the {.vignette epicdata::metadata_long}
                     vignette."),
                   call = call, class = "error.setter.variable.cats.2")
  }

  # Count if there is a single `=` in each element of cats
  if (cats %>% stringi::stri_count_regex("=") %>% magrittr::equals(1) %>%
      all() %>% magrittr::not()) {
    report <- cats[cats %>%
                     stringi::stri_count_regex("=") %>%
                     magrittr::equals(1) %>%
                     magrittr::not()] %>%
      stringi::stri_flatten(collapse = ", ")

    cli::cli_abort(c("!" = "{.var {cats_type}} has the wrong format.",
                     "i" = "{.var {cats_type}} must connect an integer with a
                     text, i.e., non-integer, via a single equal sign, e.g.,
                     {.var 1 = female}",
                     "i" = "In variable {.var {name}}, some elements have the
                     wrong format: {report}"),
                   call = call, class = "error.setter.variable.cats.3")
  }

  # Check length of cats
  if (cats %>% length() %>% magrittr::equals(1)) {
    cli::cli_abort(c("!" = "{.var {cats_type}} has `length == 1`, but must have
                     `length > 1`.",
                     "i" = "You specified only a single category for variable
                     {.var {name}}."),
                   call = call, class = "error.setter.variable.cats.10")
  }

  # Split each element of cats in two elements based on the `=`, also trim
  splitted <- cats %>%
    stringi::stri_split_regex(pattern = "=", simplify = TRUE) %>%
    apply(2, stringi::stri_trim_both) %>%
    as.data.frame()

  # Make sure that `splitted` has exactly two columns
  if (splitted %>% ncol() %>% magrittr::equals(2) %>% magrittr::not()) {
    cli::cli_abort(c("x" = "Error IE001"), call = call, .internal = TRUE,
                   class = "IE001")
  }

  # Derive which elements are integers
  suppressWarnings({
    inti <- splitted %>%
      sapply(as.integer) %>%
      as.data.frame()
  })

  # Check for doubles
  inti_num <- inti %>% unlist() %>% as.numeric()
  suppressWarnings({
    splitted_num <- splitted %>% unlist() %>% as.numeric()
  })

  if (inti_num %>%
      magrittr::equals(splitted_num) %>%
      magrittr::extract(!is.na(.)) %>%
      all() %>%
      magrittr::not()) {
    report <- splitted_num[inti_num != splitted_num] %>%
      magrittr::extract(!is.na(.)) %>%
      as.character() %>%
      unique() %>%
      stringi::stri_flatten(collapse = ", ")

    cli::cli_abort(c("!" = "{.var {cats_type}} must not contain doubles.",
                     "i" = "In variable {.var {name}}, some elements are
                     double: {report}"),
                   call = call, class = "error.setter.variable.cats.4")
  }

  # Error if there is no integer
  if (inti %>%
      is.na() %>%
      rowSums() %>%
      magrittr::equals(2) %>%
      any()) {
    report <- splitted[inti %>%
                         is.na() %>%
                         rowSums() %>%
                         magrittr::equals(2),] %>%
      apply(1, stringi::stri_flatten, collapse = " = ") %>%
      stringi::stri_flatten(collapse = ", ")

    cli::cli_abort(c("!" = "{.var {cats_type}} must connect an integer with a
                     text, i.e., non-integer, via a single equal sign, e.g.,
                     {.var 1 = female}",
                     "i" = "In variable {.var {name}}, some elements of
                     {.var {cats_type}} do not contain an integer: {report}"),
                   call = call, class = "error.setter.variable.cats.5"
    )
  }

  # Error if there are two integers
  if (inti %>%
      is.na() %>%
      rowSums() %>%
      magrittr::equals(0) %>%
      any()) {
    report <- splitted[inti %>%
                         is.na() %>%
                         rowSums() %>%
                         magrittr::equals(0),] %>%
      apply(1, stringi::stri_flatten, collapse = " = ") %>%
      stringi::stri_flatten(collapse = ", ")

    cli::cli_abort(c("!" = "{.var {cats_type}} must connect an integer with a
                     text, i.e., non-integer, via an equal sign, e.g.,
                     {.var 1 = female}",
                     "i" = "In variable {.var {name}}, some elements of
                     {.var {cats_type}} contain two integers: {report}"),
                   call = call, class = "error.setter.variable.cats.6"
    )
  }

  # Process if exactly one is integer
  if (inti %>%
      is.na() %>%
      rowSums() %>%
      magrittr::equals(1) %>%
      all()) {
    out <- data.frame(level = rep(NA,nrow(splitted)),
                      label = rep(NA,nrow(splitted)))
    for (i in 1:nrow(out)) {
      out$label[i] <- splitted[i, inti[i,] %>% is.na() %>% which()]
      out$level[i] <- splitted[i, inti[i,] %>% is.na() %>% magrittr::not() %>%
                                 which()]
    }

    out$level %<>% as.integer()

    # No NAs after tranformation to integer
    if (out$level %>% is.na() %>% any()) {
      cli::cli_abort(c("x" = "Error IE003"), call = call, .internal = TRUE,
                     class = "IE003")
    }

    # Error if negative integers (0 is allowed)
    if (out$level %>% magrittr::is_less_than(0) %>% any()) {
      cli::cli_abort(c("!" = "{.var {cats_type}} must not contain negative
                       integers.",
                       "i" = "Variable {.var {name}} contains negative
                       integers."),
                     call = call, class = "error.setter.variable.cats.7"
      )
    }

    # Error if duplcicated integers
    if (out$level %>% duplicated() %>% any()) {
      report <- out$level[out$level %>% duplicated() %>% any()] %>%
        as.character() %>%
        unique() %>%
        stringi::stri_flatten(collapse = ", ")

      cli::cli_abort(c("!" = "{.var {cats_type}} must not contain duplicated
                       values.",
                       "i" = "Variable {.var {name}} contains duplicated
                       integers: {report}"),
                     call = call, class = "error.setter.variable.cats.8")
    }

    # Error if duplicated levels
    if (out$label %>% duplicated() %>% any()) {
      report <- out$label[out$label %>% duplicated() %>% any()] %>%
        unique() %>%
        stringi::stri_flatten(collapse = ", ")

      cli::cli_abort(c("!" = "{.var {cats_type}} must not contain duplicated
                       values.",
                       "i" = "Variable {.var {name}} contains duplicated
                       labels: {report}"),
                     call = call, class = "error.setter.variable.cats.9")
    }

    # Order by integer
    out <- out[out$level %>% order(),] %>% magrittr::set_rownames(1:nrow(out))
  } else {
    cli::cli_abort(c("x" = "Error IE002"), call = call, .internal = TRUE,
                   class = "IE002")
  }

  # Return
  out
}

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
setter.variable.process.inputs <- function(value) {
  for (i in seq_along(value)) {
    # Process cats input
    if (!is.data.frame(value[[i]]$cats)) {
      value[[i]]$cats <- setter.variable.cats(cats = value[[i]]$cats,
                                              name = value[[i]]$var.name,
                                              eng = FALSE)
    }
    # Process cats.eng input
    if (!is.data.frame(value[[i]]$cats.eng)) {
      value[[i]]$cats.eng <- setter.variable.cats(cats = value[[i]]$cats.eng,
                                                  name = value[[i]]$var.name,
                                                  eng = TRUE)
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
    # Start with change == FALSE, just for safety
    change <- FALSE

    # Check if the value for group in variable i actually changed
    if (is.null(self@var.list[[i]][["group"]]) & is.null(value[[i]][["group"]])) {
      change <- FALSE
    } else if (is.null(self@var.list[[i]][["group"]]) & !is.null(value[[i]][["group"]])) {
      change <- TRUE
    } else if (!is.null(self@var.list[[i]][["group"]]) & is.null(value[[i]][["group"]])) {
      change <- TRUE
    } else if (!is.null(self@var.list[[i]][["group"]]) & !is.null(value[[i]][["group"]])) {
      if (self@var.list[[i]][["group"]] == value[[i]][["group"]]) {
        change <- FALSE
      } else {
        change <- TRUE
      }
    }

    # Only make adjustments if the value for group actually changed
    if (change) {
      # If group is now NULL, turn all .default.group elements to NULL as well
      if (is.null(value[[i]][["group"]])) {
        value[[i]]$touch.na.default.group <- NULL
        # ADD MORE DEFAULTS HERE
      } else {
        # Check if the new group actually exists in var.groups
        if (value[[i]][["group"]] %in% group_names) {
          # If the new group exists, search for the correct group
          for (j in seq_along(self@var.groups)) {
            if (self@var.groups[[j]][["group.name"]] == value[[i]][["group"]]) {
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
  # Define pre-defined defaults
  pre.defined.touch.na <- TRUE
  # ADD MORE DEFAULTS HERE

  # Apply defaults
  for (i in seq_along(value)) {
    # touch.na
    if (!is.null(value[[i]][["touch.na"]])) {
      value[[i]][["touch.na.final"]] <- value[[i]][["touch.na"]]
    } else {
      if (!is.null(value[[i]][["touch.na.default.group"]])) {
        value[[i]][["touch.na.final"]] <- value[[i]][["touch.na.default.group"]]
      } else {
        if (!is.null(value[[i]][["touch.na.default.option"]])) {
          value[[i]][["touch.na.final"]] <- value[[i]][["touch.na.default.option"]]
        } else {
          value[[i]][["touch.na.final"]] <- pre.defined.touch.na
        }
      }
    }

    # ADD MORE DEFAULTS HERE
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
      # Check if group has been specified
      if (!is.null(updated.var.list[[j]][["group"]])) {
        # Check if the specified group is the current group
        if (updated.var.list[[j]][["group"]] == current_group) {
          # Update .default.group values
          updated.var.list[[j]][["touch.na.default.group"]] <- value[[current_group]][["touch.na"]]
          # ADD MORE DEFAULTS HERE
        }
      }
    }
  }

  # Return
  updated.var.list
}


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
