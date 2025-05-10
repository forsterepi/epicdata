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
process.cats <- function(cats, name, eng = FALSE,
                                 call = rlang::caller_env()) {
  # Check input: eng
  if (!is.logical(eng) | is.na(eng)) {
    cli::cli_abort(c("x" = "Error IE004"), .internal = TRUE, class = "IE004")
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
                   call = call, class = "error.process.cats.1")
  }

  # cats must be character
  if (!is.character(cats)) {
    cli::cli_abort(c("!" = "{.var {cats_type}} must be specified as character
                     vector.",
                     "i" = "Applies to variable {.var {name}}.",
                     "i" = "See the {.vignette epicdata::metadata_long}
                     vignette."),
                   call = call, class = "error.process.cats.2")
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
                   call = call, class = "error.process.cats.3")
  }

  # Check length of cats
  if (cats %>% length() %>% magrittr::equals(1)) {
    cli::cli_abort(c("!" = "{.var {cats_type}} has `length == 1`, but must have
                     `length > 1`.",
                     "i" = "You specified only a single category for variable
                     {.var {name}}."),
                   call = call, class = "error.process.cats.10")
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
                   call = call, class = "error.process.cats.4")
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
                   call = call, class = "error.process.cats.5"
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
                   call = call, class = "error.process.cats.6"
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
      cli::cli_abort(c("x" = "Error IE003"), .internal = TRUE, class = "IE003")
    }

    # Error if negative integers (0 is allowed)
    if (out$level %>% magrittr::is_less_than(0) %>% any()) {
      cli::cli_abort(c("!" = "{.var {cats_type}} must not contain negative
                       integers.",
                       "i" = "Variable {.var {name}} contains negative
                       integers."),
                     call = call, class = "error.process.cats.7"
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
                     call = call, class = "error.process.cats.8")
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
                     call = call, class = "error.process.cats.9")
    }

    # Order by integer
    out <- out[out$level %>% order(),] %>% magrittr::set_rownames(1:nrow(out))
  } else {
    cli::cli_abort(c("x" = "Error IE002"), .internal = TRUE, class = "IE002")
  }

  # Return
  out
}

#' Process `dict` and `dict.eng`
#'
#' Process the input values for the dictionnairy key `dict` and its English
#' translation.
#'
#' @param dict The inputted character vector.
#'
#' @returns A list of two named elements: `out.dict` is the processed value of
#' key `dict`, `out.dict.eng` is the processed input of `dict.eng`.
#'
#' @noRd
process.dict <- function(dict, call = rlang::caller_env()) {
  # Return NULL if dict is NULL
  if (is.null(dict)) {
    return(NULL)
  }

  # Check input
  if (!checkmate::test_character(dict, min.len = 1, min.chars = 1,
                                 any.missing = FALSE, null.ok = FALSE)) {
    cli::cli_abort("Key {.var dict} must be a character vector.",
                   call = call, class = "error.process.dict.1")
  }

  # Parse
  out.dict <- rep(NA_character_, length(dict))
  out.dict.eng <- rep(NA_character_, length(dict))

  for (i in seq_along(dict)) {
    rlang::try_fetch(ex <- rlang::parse_expr(dict[i]),
      error = function(cnd) {
        cli::cli_abort(c("Unable to process `dict` element {.var {dict[i]}}",
          "i" = "Please check the required format in the documentation:
          {.vignette epicdata::metadata_long}"),
          call = call, class = "error.process.dict.2")})

    if (identical(length(ex), 1L)) {
      if (is.symbol(ex)) {
        out.dict[i] <- rlang::expr_text(ex)
        out.dict.eng[i] <- "-"
        names(out.dict.eng)[i] <- rlang::expr_text(ex)
      } else {
        cli::cli_abort(c("`dict` element {.var {dict[i]}} has an incorrect format.",
          "i" = "Please check the required format in the documentation:
          {.vignette epicdata::metadata_long}"),
          call = call, class = "error.process.dict.3")
      }
    } else if (identical(length(ex), 3L)) {
      if (identical(ex[[1]], rlang::sym("=")) &
          is.symbol(ex[[2]]) &
          (is.symbol(ex[[3]]) | rlang::is_syntactic_literal(ex[[3]]))) {
        out.dict[i] <- rlang::expr_text(ex[[2]])
        out.dict.eng[i] <- rlang::expr_text(ex[[3]]) %>%
          stringi::stri_replace_all(replacement = "", fixed = '\"')
        names(out.dict.eng)[i] <- rlang::expr_text(ex[[2]])
      } else {
        cli::cli_abort(c("`dict` element {.var {dict[i]}} has an incorrect format.",
          "i" = "Please check the required format in the documentation:
          {.vignette epicdata::metadata_long}"),
          call = call, class = "error.process.dict.4")
      }
    } else {
      cli::cli_abort(c("`dict` element {.var {dict[i]}} has an incorrect format.",
          "i" = "Please check the required format in the documentation:
          {.vignette epicdata::metadata_long}"),
          call = call, class = "error.process.dict.5")
    }
  }

  # Return
  list(out.dict = out.dict, out.dict.eng = out.dict.eng)
}

#' Process rules for handling `dict` violations
#'
#' Instructions provided in rules format are processed into the expression
#' that applies these rules.
#'
#' @param rule A character vector containing the input of `dict.rules`
#' @param dict The processed value of key `dict`, i.e., element `out.dict` from
#' the output of function `process.dict()`.
#' @param var A single string containing the variable name.
#' @param var.names A character vector containing all variables in `var.list`,
#' which are the ones that are allowed in the rule conditions.
#'
#' @returns An expression of a call to `dplyr::mutate()` with
#' `dplyr::case_when()` which applies the rules to the corresponding variable
#' when evaluated. The name of the data.frame on which `mutate()` is applied is
#' assumed to be `df`.
#'
#' @noRd
process.dict.rules <- function(rule, dict, var, var.names,
                               call = rlang::caller_env()) {
  # Ruturn NULL if rule is NULL
  if (is.null(rule)) {
    return(NULL)
  }

  # Rules are NULL without dict
  if (is.null(dict)) {
    return(NULL)
  }

  # Check the dict.rules are in rules format and return expressions
  e <- check.rules.format(rule = rule, var.names = var.names)

  # Check assigned constants (must be in dect or NA)
  for (i in seq_along(e)) {
    if (e[[i]][[length(e[[i]])]] %!in% c(dict,NA)) {
      cli::cli_abort(c("Rule {.var {rlang::expr_text(e[[i]])}} has an incorrect
        format.",
        "i" = "Value after `~` must be in `dect` or NA.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"),
      call = call, class = "error.process.dict.rules.1")
    }
  }

  # Get variable
  v <- rlang::ensym(var)

  # Add condition that value is not in dict
  dict_cond <- rlang::expr(!!v %!in% !!dict)
  ## Empty container
  e_dict <- vector(mode = "list", length = length(e))
  ## Add based on availability of other conditions
  for (i in seq_along(e)) {
    if (identical(length(e[[i]]), 3L)) {
      e_dict[[i]] <- rlang::expr(!!dict_cond & !!e[[i]][[2]] ~ !!e[[i]][[3]])
    }
    if (identical(length(e[[i]]), 2L)) {
      e_dict[[i]] <- rlang::expr(!!dict_cond ~ !!e[[i]][[2]])
    }
  }

  # Create final call by unquoting variable name v and rules e_dict
  out <- rlang::expr(df %<>% dplyr::mutate(!!v := dplyr::case_when(!!!e_dict,
            .default = !!v)))

  # Return
  out
}

#' Process variable definitions in `new`
#'
#' Instructions provided in `new` are processed into the expression that
#' creates the variable.
#'
#' @param new A character vector containing the inputted variable definition.
#' @param var A single string containing the variable name.
#' @param var.names A character vector containing all variables in `var.list`.
#' @param new.order A character vector that gives the order in which the
#' variables with key `new` are created. Is equal to the order in which they
#' appear in `var.list`.
#'
#' @returns An expression of a call to `dplyr::mutate()` which applies
#' variable definition of the corresponding variable. The name of the data.frame
#' on which `mutate()` is applied is assumed to be `df`.
#'
#' @noRd
process.new <- function(new, var, var.names, new.order) {
  # Return NULL if new is NULL
  if (is.null(new)) {
    return(NULL)
  }

  # Check the format of new and return expression
  e <- check.new.format(new = new, var = var, var.names = var.names,
                        new.order = new.order)

  # Get variable
  v <- rlang::ensym(var)

  # Create final call by unquoting variable name v and definition e
  out <- rlang::expr(df %<>% dplyr::mutate(!!v := !!e))

  # Return
  out
}


# Helpers -----------------------------------------------------------------

#' Check format requirements for rules
#'
#' Contains checks that apply to all `.rules` key, i.e., `na.rules`,
#' `dict.rules`, `cats.rules`, `limit.rules`, as well as contradictions.
#'
#' @param rule A character vector containing the inputted rules.
#' @param var.names A character vector containing all variables in `var.list`,
#' which are the ones that are allowed in the rule conditions.
#'
#' @returns If no error is thrown due to incorrect formats, returns a list of
#' expressions after parsing `rule`.
#'
#' @noRd
check.rules.format <- function(rule, var.names, call = rlang::caller_env()) {
  if (!checkmate::test_character(rule, min.len = 1, min.chars = 1,
                                 any.missing = FALSE, null.ok = FALSE)) {
    cli::cli_abort(c("x" = "Error IE006"), .internal = TRUE, class = "IE006")
  }

  erule <- vector(mode = "list", length = length(rule))
  n_length2 <- 0

  for (i in seq_along(rule)) {
    # Evaluate if rule can be parsed to an expression
    rlang::try_fetch(ex <- rlang::parse_expr(rule[i]),
      error = function(cnd) {
        cli::cli_abort(c("Unable to process rule {.var {rule[i]}}",
          "i" = "Please check the required format in the documentation:
          {.vignette epicdata::metadata_long}"),
          call = call, class = "error.check.rules.format.1")})

    # Error if evaluation is only a constant or symbol
    if (!is.call(ex)) {
      cli::cli_abort(c("Rule {.var {rule[i]}} has an incorrect format.",
        "i" = "Use of `~` required.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"),
        call = call, class = "error.check.rules.format.2")
    }

    # Error if call is not of function `~`
    if (!identical(ex[[1]], rlang::sym("~"))) {
      cli::cli_abort(c("Rule {.var {rule[i]}} has an incorrect format.",
        "i" = "Use of `~` required.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"),
        call = call, class = "error.check.rules.format.3")
    }

    # Check elements of the call to `~`
    if (identical(length(ex), 3L)) {
      # e.g., var1 > 4 & var2 == 3 ~ "value"
      constant_at <- 3

      # Value in front of ~ must be a call
      if (is.call(ex[[2]])) {
        symbols <- extract.symbols.from.ast(ex, call = call)
        if (symbols %>% magrittr::is_in(var.names) %>% all() %>%
            magrittr::not()) {
          re <- symbols[symbols %>% magrittr::is_in(var.names) %>%
                          magrittr::not()] %>% unique() %>%
            stringi::stri_c(collapse = ", ")
          cli::cli_abort(c("Rule {.var {rule[i]}} has an incorrect format.",
            "i" = "Some variable names have not been specified in `var.list`: {re}",
            "i" = "Please check the required format in the documentation:
            {.vignette epicdata::metadata_long}"),
            call = call, class = "error.check.rules.format.4")
        }
      }
    } else if (identical(length(ex), 2L)) {
      # ~ "default value"
      constant_at <- 2
      n_length2 <- n_length2 + 1
    } else {
      cli::cli_abort(c("Rule {.var {rule[i]}} has an incorrect format.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"),
        call = call, class = "error.check.rules.format.5")
    }

    # Check constant
    ## Value after ~ must be a constant
    if (!rlang::is_syntactic_literal(ex[[constant_at]])) {
      cli::cli_abort(c("Rules have an incorrect format.",
        "i" = "Value after ~ must be a constant, i.e., atomic vector or `NA`.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"),
        call = call, class = "error.check.rules.format.6")
    }
    ## Value after ~ must not be NULL
    if (is.null(ex[[constant_at]])) {
      cli::cli_abort(c("Rule {.var {rule[i]}} has an incorrect format.",
        "i" = "Value after ~ must not be `NULL`.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"),
        call = call, class = "error.check.rules.format.7")
    }


    erule[[i]] <- ex
  }

  # Only one rule can have format "~ value"
  if (n_length2 > 1) {
    cli::cli_abort(c("Defaults, i.e., rules without value in front of `~`, can
      only be assigned once.",
      "i" = "Please check the required format in the documentation:
      {.vignette epicdata::metadata_long}"),
      call = call, class = "error.check.rules.format.8")
  }

  # Reorder an put entry with "~ value" format at the end
  if (n_length2 == 1) {
    erule %<>% magrittr::extract(lapply(., length) %>%
                                   unlist() %>%
                                   order(decreasing = TRUE))
  }

  # Return
  erule
}


#' Check format requirements for key `new`
#'
#' @param new A character vector containing the inputted variable definition.
#' @param var A single string containing the variable name.
#' @param var.names A character vector containing all variables in `var.list`.
#' @param new.order A character vector that gives the order in which the
#' variables with key `new` are created. Is equal to the order in which they
#' appear in `var.list`.
#'
#' @returns If no error is thrown due to incorrect formats, returns an
#' expressions after parsing `new`.
#'
#' @noRd
check.new.format <- function(new = NULL, var = NULL, var.names = NULL,
                             new.order = NULL) {
  # Evaluate inputs
  if (!checkmate::test_character(var, len = 1, min.chars = 1,
                                 any.missing = FALSE, null.ok = FALSE)) {
    cli::cli_abort(c("x" = "Error IE007"), .internal = TRUE, class = "IE007")
  }
  if (!checkmate::test_character(var.names, min.len = 1, min.chars = 1,
                                 any.missing = FALSE, null.ok = FALSE)) {
    cli::cli_abort(c("x" = "Error IE008"), .internal = TRUE, class = "IE008")
  }
  if (!checkmate::test_character(new.order, min.len = 1, min.chars = 1,
                                 any.missing = FALSE, null.ok = FALSE)) {
    cli::cli_abort(c("x" = "Error IE009"), .internal = TRUE, class = "IE009")
  }

  # Craft call
  call <- craft.call.var.list(variable = var, key = "new", value = new)

  # Allow numeric and boolean constants
  if (checkmate::test_scalar(new, na.ok = FALSE, null.ok = FALSE)) {
    new %<>% as.character()
  }

  # Check character input
  if (!checkmate::test_character(new, len = 1, min.chars = 1,
                                 any.missing = FALSE, null.ok = FALSE)) {
    cli::cli_abort(c("For key `new`, the variable definition must be provided
      as a single character.",
      "i" = "Please check the required format in the documentation:
      {.vignette epicdata::metadata_long}"), call = call,
      class = "error.check.new.format.1")
  }

  # Evaluate if rule can be parsed to an expression
  rlang::try_fetch(ex <- rlang::parse_expr(new),
    error = function(cnd) {
      cli::cli_abort(c("Unable to process variable definition {.var {new}}",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"), call = call,
        class = "error.check.new.format.2")})

  # Error if call is of function `=`
  if (length(ex) > 1) {
    if (identical(ex[[1]], rlang::sym("="))) {
      cli::cli_abort(c("Variable definition {.var {new}} has an incorrect format.",
        "i" = "Do not assign the variable name with `=`.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"), call = call,
        class = "error.check.new.format.3")
    }
  }

  # Error if only NULL
  if (identical(length(ex), 1L)) {
    if (is.null(ex)) {
      cli::cli_abort(c("Variable definition {.var {new}} has an incorrect format.",
        "i" = "Do not assign `NULL`.",
        "i" = "Please check the required format in the documentation:
        {.vignette epicdata::metadata_long}"), call = call,
        class = "error.check.new.format.4")
    }
  }

  # Check if all symbols are variables that are vailable
  symbols <- extract.symbols.from.ast(ex, call = call)
  not_new <- var.names[var.names %!in% new.order]

  if (identical(which(new.order == var), 1L)) {
    allowed <- not_new
  } else {
    allowed <- c(not_new, new.order[1:(which(new.order == var) - 1)])
  }

  if (symbols %>% magrittr::is_in(allowed) %>% all() %>%  magrittr::not()) {
    re <- symbols[symbols %>% magrittr::is_in(allowed) %>%
                  magrittr::not()] %>% unique() %>%
                  stringi::stri_c(collapse = ", ")
    cli::cli_abort(c("Variable definition {.var {new}} has an incorrect format.",
      "i" = "Some variable names are not available for defining {.var {var}}: {re}",
      "i" = "Please check the required format in the documentation:
      {.vignette epicdata::metadata_long}"), call = call,
      class = "error.check.new.format.4")
  }

  # Return
  ex
}

#' Extract symbols from AST
#'
#' Extract all symbols apart from function names from abstract syntax trees.
#'
#' @param x An expression.
#'
#' @returns A character vector containing all symbol names. If no symobl are in
#' the call, returns an empty character vector, i.e., character(0).
#'
#' @noRd
extract.symbols.from.ast <- function(x, call = rlang::caller_env()) {
  if (rlang::is_syntactic_literal(x)) {
    # If x is a constant, return an empty character vector
    character(0)
  } else if (is.symbol(x)) {
    # If x is a symbol, return the symbol as string
    rlang::as_string(x)
  } else if (is.call(x)) {
    # If x is a call, apply the function to all elements of the call apart from
    # the first one, which contains the function name. This also avoids that
    # package names and function are included when `::` is used, because the
    # call to `::` is also in the first position instead of the function name.
    # unlist() puts the individual elements together. as.character() avoids
    # problems when some elements have names, e.g., function arguments with
    # names.
    lapply(x[-1], extract.symbols.from.ast, call = call) %>%
      unlist() %>% as.character()
  } else if (is.pairlist(x)) {
    # Same as call
    lapply(x[-1], extract.symbols.from.ast, call = call) %>%
      unlist() %>% as.character()
  } else {
    # Throw internal error for other types
    cli::cli_abort(c("x" = "Error IE14121"), call = call, .internal = TRUE,
                   class = "IE14121")
  }
}

#' Craft call for error messages when
#'
#' Desc.
#'
#' @param x Desc.
#'
#' @returns Desc.
#'
#' @noRd
craft.call.var.list <- function(variable, key, value,
                                call = rlang::caller_env()) {
  # Check inputs


  t <- rlang::trace_back()

  if (identical(t$call[[1]][[1]], rlang::sym("@<-"))) {
    var <- variable
    v1 <- rlang::ensym(var)
    ke <- key
    k <- rlang::ensym(ke)
    val <- value
    v2 <- rlang::enexpr(val)
    call <- rlang::expr(`$`(`$`(metadata@var.list, !!v1), !!k) <- !!v2)
  } else if (identical(t$call[[1]][[1]], rlang::sym("metadata"))) {
    call <- t$call[[1]]
  } else {
    call <- rlang::caller_env()
  }

  call
}
