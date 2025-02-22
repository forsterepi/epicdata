yaml.read <- function(x, arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {

  rlang::try_fetch({
    yaml_input <- yaml::read_yaml(x)
  },
  error = function(cnd) {
    cli::cli_abort(c("Loading YAML metadata failed!",
                     c("Loading failed due to an error in the YAML grammar. The
                     {.code yaml::read_yaml()} error message below reports its
                     location.",
                      cnd$message,
                      "Did you forget quotation marks around
                      {.var id.pattern}?") %>%
                       magrittr::set_names(c("i","x","i")) %>%
                       magrittr::extract(c(
                         stringr::str_detect(cnd$message,"line"),
                         TRUE,
                         stringr::str_detect(cnd$message,"line"))
                         )), # add info only if applicable
                   call = call,
                   class = "error.yaml.read.1")})

  yaml_input
}

yaml.str.input <- function(x, arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  logger::log_info("Checking basic metadata input structure",
                   namespace = "epicdata")

  rlang::try_fetch({
    # START: actual check
    checkmate::assert_list(x, min.len = 1, names = "unique", null.ok = FALSE,
                           .var.name = arg)
    # END: actual check
    },
   error = function(cnd) {
    logger::log_error("Checking basic metadata input structure failed",
                      namespace = "epicdata")
    logger::log_debug(cnd$message, namespace = "epicdata")
    cli::cli_abort(c("YAML must describe a single list!",
                     "x" = cnd$message,
                     "i" = "Do not use dashes ({.var -}) in front of the
                            main metadata components: options, var.list, etc."),
                   call = call,
                   class = "error.yaml.str.input.1")})
}


yaml.str.component <- function(x, arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {

  components.list <- c("options","var.list","na.codes","na.rules","mc.sets",
                       "dict","contras")

  rlang::try_fetch({
    # START: actual check
    checkmate::assert_subset(names(x), components.list, .var.name = arg)
    # END: actual check
  },
  error = function(cnd) {
    cli::cli_abort(c("YAML contains invalid component names!",
                     cnd$message %>%
                       stringr::str_replace_all("\\{","(") %>%
                       stringr::str_replace_all("\\}",")"),
                     "Only the above listed components can be on the first
                     YAML layer, i.e., without indentation.") %>%
                     magrittr::set_names(c("!","x","i")),
                   call = call,
                   class = "error.yaml.str.component.1")})
}

yaml.str.options <- function(x, call = rlang::caller_env()) {

  # Create empty list

  options.list <- c("study.name",
                    "id.var", "id.pattern",
                    "load.from", "mc.handling", "double.entry",
                    "date.format", "format.date",
                    "time.format", "format.time",
                    "datetime.format", "format.datetime",
                    "to.na", "touch.na",
                    "to.factor",
                    "translate")

  check.options.template <- vector(mode = "list",
                                   length = length(options.list)) %>%
                            magrittr::set_names(options.list)


  # Define checks for every option

  check.options.template[["study.name"]] <- rlang::expr(
    checkmate::assert_string(x$options$study.name, min.chars = 1,
                             na.ok = FALSE, null.ok = TRUE, add = coll,
                             .var.name = "study.name"))

  check.options.template[["id.var"]] <- rlang::expr(
    checkmate::assert_string(x$options$id.var, min.chars = 1,
                             na.ok = FALSE, null.ok = TRUE, add = coll,
                             .var.name = "id.var"))

  check.options.template[["id.pattern"]] <- rlang::expr(
    checkmate::assert_string(x$options$id.pattern, min.chars = 1,
                             na.ok = FALSE, null.ok = TRUE, add = coll,
                             .var.name = "id.pattern"))


  # Select

  options.used <- names(x$options)
  check.options <- check.options.template[options.used]


  # Eval

  rlang::try_fetch({
    # START: actual check
    coll <- checkmate::makeAssertCollection()
    for (i in seq_along(check.options)) {
      eval(check.options[[i]])
    }
    checkmate::reportAssertions(coll)
    # END: actual check
  },
  error = function(cnd) {
    cli::cli_abort(c("Some options have not been specified correctly.",
                     cnd$message %>% stringr::str_split_1("\n") %>%
                       stringr::str_trim("both") %>%
                       magrittr::extract(2:length(.)) %>%
                       stringr::str_replace("^\\* Variable","Option") %>%
                       magrittr::set_names(rep("x",length(.))),
                     "i" = "Some info"),
                   call = call,
                   class = "error.yaml.str.options.1")})

}

yaml.forgive.component.name <- function(x) {

  names(x) %<>% stringr::str_to_lower()

  forgive.options <- c("option")
  forgive.var.list <- c("varlist","varslist","vars.list","list.var","list.vars",
                        "variable.list","variables.list")
  forgive.na.codes <- c("codes.na")
  forgive.na.rules <- c("rules.na")
  forgive.mc.sets <- c("sets.mc")
  forgive.dict <- c("dictionary")
  forgive.contras <- c("contra","contradiction","contradictions")

  names(x)[names(x) %in% forgive.options] <- "options"
  names(x)[names(x) %in% forgive.var.list] <- "var.list"
  names(x)[names(x) %in% forgive.na.codes] <- "na.codes"
  names(x)[names(x) %in% forgive.na.rules] <- "na.rules"
  names(x)[names(x) %in% forgive.mc.sets] <- "mc.sets"
  names(x)[names(x) %in% forgive.dict] <- "dict"
  names(x)[names(x) %in% forgive.contras] <- "contras"

  x
}





#' Check basic structure of YAML
#'
#' Check the basic structure after reading in the metadata yaml and before
#' processing any of it.
#'
#' @param x A list, namely the read-in metadata yaml file before processing.
#'
#' @noRd
yaml.check.structure <- function(x) {
  # Define names
  metadata.components <- c("options","var.list","na.codes","na.rules","mc.sets",
                           "dict","contras")
  ## options
  options.list <- c("study.name", "id.var", "load.from",
                    "date.format", "format.date",
                    "time.format", "format.time",
                    "datetime.format", "format.datetime",
                    "mc.handling", "double.entry", "translate", "id.pattern")
  options.load.from <- c("folder", "db")
  options.mc.handling <- c("none", "adjust")
  ## var.list
  var.list.vars <- c("id", "label", "label.eng", "type", "na.else", "ops",
                     "cats", "cats.eng", "from", "from.exclude", "to",
                     "to.exclude", "format", "new")
  var.list.types <- c("text", "cat", "num", "date", "time", "datetime")
  ## na.codes
  na.codes.vars <- c("type", "code", "label")
  na.codes.types <- c("MISSING", "M", "JUMP", "J")
  ## operators
  text.cat.operators <- c("IN", "!IN")
  num.date.operators <- c("==", "!=", ">=", ">", "<=", "<")

  # Check components
  ## Basic structure
  rlang::try_fetch(checkmate::assert_list(x, types = c("list", "character"),
                                          names = "unique"),
                   error = function(cnd) {
                     cli::cli_abort(c("YAML must describe a single list!",
                                      "i" = "Do not use dashes ({.var -}) in front of the
                            main metadata components: options, var.list, etc."),
                                    parent = cnd, class = "yaml.strc.no.list")
                   })
  ## Names
  rlang::try_fetch(checkmate::assert_subset(names(x),
                                            choices = metadata.components,
                                            empty.ok = F),
                   error = function(cnd) {
                     incorrect.components <- names(x) %>% magrittr::extract(names(x) %>%
                                                                              magrittr::is_in(metadata.components) %>% magrittr::not()) %>%
                       stringr::str_c(collapse = ", ")
                     cli::cli_abort(c("Some metadata component names are incorrect!",
                                      "i" = "Incorrect components: {incorrect.components}"),
                                    parent = cnd, class = "yaml.strc.no.list")
                   })



}



#' Process categories in YAML
#'
#' Process category names of integer variables in var.list
#'
#' @param x A list, namely the read-in metadata yaml file after structural
#'   checks have been conducted.
#' @param cat.col.name The name of the field in yaml below var.list.
#'
#' @noRd
yaml.prc.var.list.cats <- function(x, cat.col.name) {
  # Check input
  checkmate::assert_character(cat.col.name, min.chars = 1, len = 1,
                              null.ok = F, any.missing = F)
  #=============================================================================
  for (i in 1:length(x$var.list)) {
    if (cat.col.name %>% magrittr::is_in(names(x$var.list[[i]]))) {
      # Transform yaml like
      # values:
      # - 1: one or more
      # - 2: two
      # - 3: three
      # OR
      # values:
      # - 1: one or more
      #   2: two
      #   3: three
      # INTO
      # values:
      #   1: one or more
      #   2: two
      #   3: three
      if (checkmate::test_list(x$var.list[[i]][[cat.col.name]],
                               types = "list")) {
        x$var.list[[i]][[cat.col.name]] %<>% yum::flatten_list_of_lists()
      }

      # Transform yaml like
      # values:
      #   1: one or more
      #   2: two
      #   3: three
      # INTO
      # values:
      # - 1=one or more
      # - 2=two
      # - 3=three
      if (checkmate::test_list(x$var.list[[i]][[cat.col.name]],
                               types = "character")) {
        out <- vector(mode = "character",
                      length = length(x$var.list[[i]][[cat.col.name]]))
        for (j in 1:length(out)) {
          out[j] <- paste0(names(x$var.list[[i]][[cat.col.name]])[j],"=",
                           x$var.list[[i]][[cat.col.name]][[j]])
        }
        x$var.list[[i]][[cat.col.name]] <- out
      }

      # Transform yaml like
      # values:
      # - 1=one or more
      # - 2=two
      # - 3=three
      # INTO
      # values: 1=one or more|2=two|3=three
      if (checkmate::test_character(x$var.list[[i]][[cat.col.name]])) {
        x$var.list[[i]][[cat.col.name]] %<>%
          stringr::str_c(collapse = "|") %>%
          stringr::str_replace_all("[:blank:]*=[:blank:]*","=")
      }
    }
  }

  return(x)
}

# creating new variables with mutate
#var_name <- "log_var3"
#new_value <- "log(var3) + 5"
#x <- data.frame(var3 = c(1:5))
#x %>% dplyr::mutate("{var_name}" := eval(parse(text = new_value)))
# mutate all variables with new: field from top to bottom
# before re-arrange var.list in case newly created variables are used to create other variables
# values not allowes in new:? NULL, across(
# to test new: expressions, create a fake dataset with the variables and corresponding types and values

# handling format for date, time, datetime:
# - if there are individual format specifications in var.list, variable format will be created
# - if there are no individual format specifications in var.list, it needs to be created
# - fill in the empty cells, where a corresponding format is needed with the values from date.format, etc.
