var.prop.name <- S7::new_property(
  class = S7::class_character,
  default = quote(stop("@name is required")),
  validator = function(value) {
    if (length(value) != 1L) {
      "must be length 1"
    } else if (make.names(value) != value) {
      "must be a syntactically valid name"
    }
  }
)

var.prop.type <- S7::new_property(
  class = S7::class_character,
  default = quote(stop("@type is required")),
  validator = function(value) {
    if (length(value) != 1L) {
      "must be length 1"
    } else if (!(value %in% c("text","cat","num","date","time","datetime"))) {
      "must be one of 'text','cat','num','date','time','datetime'"
    }
  }
)

var.prop.old.id <- S7::new_property(
  class = NULL | S7::class_character,
  validator = function(value) {
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  }
)

var.prop.label <- S7::new_property(
  class = NULL | S7::class_character,
  validator = function(value) {
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  }
)

var.prop.cats <- S7::new_property(
  class = NULL | S7::class_data.frame,
  validator = function(value) {
    if (!is.null(value)) {
      if (any(is.na(value))) {
        "must not be NA"
      }
    }
  },
  setter = function(self, value) {
    self@cats <- setter.variable.cats(value, name = self@name, eng = FALSE)
    self
  }
)

var.prop.cats.eng <- S7::new_property(
  class = NULL | S7::class_data.frame,
  setter = function(self, value) {
    self@cats.eng <- setter.variable.cats(value, name = self@name, eng = TRUE)
    self
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

  # cats must be cahracter
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
