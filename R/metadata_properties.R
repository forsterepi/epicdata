#' @include metadata_validator.R
NULL

# options

meta.prop.study.name <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    if (!(length(value) == 1L)) {
      "must have length 1"
    }
  },
  setter = function(self, value) {
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
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  }
)

meta.prop.id.pattern <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  }
  # add to validator a check for valid regex
)

meta.prop.touch.na <- S7::new_property(
  class = NULL | S7::class_logical,
  validator = function(value) {
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  },
  setter = function(self, value) {
    for (i in seq_along(self@var.list)) {
      self@var.list[[i]]$touch.na.default.option <- value
    }

    self@touch.na <- value
    self
  }
)

meta.prop.var.list <- S7::new_property(
  class = S7::class_list, # var.list cannot be NULL
  validator = var.list.validator,
  setter = function(self, value) {

    # Loop over all variables
    for (i in seq_along(value)) {
      # Make element var.name and the name of the list element consistent
      names(value)[i] <- value[[i]]$var.name

      # Process cats input
      if (!is.data.frame(value[[i]]$cats)) {
        value[[i]]$cats <- setter.variable.cats(cats = value[[i]]$cats,
                                                name = value[[i]]$var.name,
                                                eng = FALSE)
      }
    }

    # Update default.group elements when changing group
    value %<>% setter.variable.update.default.group(self)

    # Set the new value
    self@var.list <- value
    self
  }
)

meta.prop.var.names <- S7::new_property(
  getter = function(self) {
    names(self@var.list)
  }
)


meta.prop.var.groups <- S7::new_property(
  class = NULL | S7::class_list,
  validator = var.groups.validator,
  setter = function(self, value) {

    # If the last element is removed, remove the empty list as well
    if (is.list(value) & length(value) == 0) {
      value <- NULL
    }

    # Update names
    for (i in seq_along(value)) {
      # Make element group.name and the name of the list element consistent
      rlang::try_fetch({names(value)[i] <- value[[i]]$group.name},
        error = function(cnd) {
          cli::cli_abort(c("Element {.var group.name} cannot be set to {.var NULL}",
                           "i" = "To delete a group, set @var.groups$[group.name]
                           to {.var NULL}."),
                         call = rlang::caller_env(), class = "tbd")
          ## SOLVE CALLER_ENV PROBLEMS
        }
      )
    }

    # Identify changes
    changes <- setter.group.identify.changes(value, self)

    # Update based on changes
    if (!is.null(changes)) {
      # Get copy of var.list to update
      updated.var.list <- self@var.list

      # Loop over all default.group variables
      for (i in seq_along(changes)) {
        # Loop over all relevant groups
        for (j in 1:nrow(changes)) {
          # Only proceed if there has been a change
          if (changes[j,i]) {
            # Get current group name
            current_group <- rownames(changes)[j]
            # Loop over all variables
            for (k in seq_along(updated.var.list)) {
              # Check if group has been specified
              if (!is.null(updated.var.list[[k]]$group)) {
                # Check if the specified group is the current group
                if (updated.var.list[[k]]$group == current_group) {
                  # Update
                  updated.var.list[[k]]$touch.na.default.group <- value[[current_group]]$touch.na
                  # ADD MORE DEFAULTS HERE
                }
              }
            }
          }
        }
      }
      # Update var.list
      self@var.list <- updated.var.list
    }

    # Set all default.group elements in var.list to NULL if value is NULL
    if (is.null(value)) {
      for (i in seq_along(self@var.list)) {
        self@var.list[[i]]$touch.na.default.group <- NULL
      }
    }

    # Set the new values
    self@var.groups <- value
    self
  }
)

meta.prop.group.names <- S7::new_property(
  getter = function(self) {
    names(self@var.groups)
  }
)

# Modules

meta.prop.DUP_NO.ID <- S7::new_property(
  getter = function(self) {
    if (is.null(self@id.var)) {
      TRUE
    } else {
      FALSE
    }
  }
)

meta.prop.DUP_FREQ <- S7::new_property(
  getter = function(self) {
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
#' Used in the constructor of S7 class `epicdata::variable`
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

setter.variable.update.default.group <- function(value, self) {

  # Loop over all variables in the updated var.list, i.e., value
  for (i in seq_along(value)) {
    # Start with change == FALSE, just for safety
    change <- FALSE

    # Check if the value for group in variable i actually changed
    if (is.null(self@var.list[[i]]$group) & is.null(value[[i]]$group)) {
      change <- FALSE
    } else if (is.null(self@var.list[[i]]$group) & !is.null(value[[i]]$group)) {
      change <- TRUE
    } else if (!is.null(self@var.list[[i]]$group) & is.null(value[[i]]$group)) {
      change <- TRUE
    } else if (!is.null(self@var.list[[i]]$group) & !is.null(value[[i]]$group)) {
      if (self@var.list[[i]]$group == value[[i]]$group) {
        change <- FALSE
      } else {
        change <- TRUE
      }
    }

    # Only make adjustments if the value for group actually changed
    if (change) {
      ## If group is now NULL, turn all default.group elements to NULL as well
      if (is.null(value[[i]]$group)) {
        value[[i]]$touch.na.default.group <- NULL
        # ADD MORE DEFAULTS HERE
      } else {
        ### Check if the new group actually exists
        if (value[[i]]$group %in% self@group.names) {
          #### If the new group exists, search for the correct group
          for (j in seq_along(self@var.groups)) {
            if (self@var.groups[[j]]$group.name == value[[i]]$group) {
              value[[i]]$touch.na.default.group <- self@var.groups[[j]]$touch.na
              # ADD MORE DEFAULTS HERE
            }
          }
        } else {
          #### If the new group does not exist, turn the default back to NULL
          value[[i]]$touch.na.default.group <- NULL
          # ADD MORE DEFAULTS HERE
        }
      }
    }
  }

  value
}


setter.group.identify.changes <- function(value, self) {

  if (is.null(value)) {
    return(NULL)
  } else {

    # Check if groups were added or deleted (this should include name changes)
    ## Using names() below only works if this function goes after making
    ## element group.name and the name of the list element consistent
    old_names <- names(self@var.groups)
    new_names <- names(value)

    deleted_groups <- old_names[!(old_names %in% new_names)]
    new_groups <- new_names[!(new_names %in% old_names)]

    # List the elements, for which changes need to be identified
    default_group_elements <- c("touch.na") # ADD MORE DEFAULTS HERE

    # Create empty container
    all_groups <- c(new_names, deleted_groups)
    changes <- matrix(rep(FALSE,
                          length(all_groups) * length(default_group_elements)),
                      nrow = length(all_groups),
                      ncol = length(default_group_elements)) %>%
      as.data.frame() %>%
      magrittr::set_colnames(default_group_elements) %>%
      magrittr::set_rownames(all_groups)

    # Loop over all groups
    for (i in seq_along(changes)) {
      for (j in 1:nrow(changes)) {
        current_group <- rownames(changes)[j]
        if (current_group %in% c(deleted_groups, new_groups)) {
          changes[j,i] <- TRUE
        } else {
          if (is.null(self@var.groups[[current_group]]$touch.na) &
              is.null(value[[current_group]]$touch.na)) {
            changes[j,i] <- FALSE
          } else if (is.null(self@var.groups[[current_group]]$touch.na) &
                     !is.null(value[[current_group]]$touch.na)) {
            changes[j,i] <- TRUE
          } else if (!is.null(self@var.groups[[current_group]]$touch.na) &
                     is.null(value[[current_group]]$touch.na)) {
            changes[j,i] <- TRUE
          } else if (!is.null(self@var.groups[[current_group]]$touch.na) &
                     !is.null(value[[current_group]]$touch.na)) {
            if (self@var.groups[[current_group]]$touch.na == value[[current_group]]$touch.na) {
              changes[j,i] <- FALSE
            } else {
              changes[j,i] <- TRUE
            }
          }
          # ADD MORE DEFAULTS HERE
        }

      }
    }
  }

  return(changes)
}
