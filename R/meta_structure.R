meta.read <- function(x, arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {

  rlang::try_fetch({
    yaml_input <- yaml::read_yaml(x)
  },
  error = function(cnd) {
    cli::cli_abort(c("Loading YAML metadata failed!",
                     c("Loading failed due to an error in the YAML grammar. The
                     {.code yaml::read_yaml()} error message below reports its
                     location.", cnd$message,
                      "Did you forget quotation marks around
                      {.var id.pattern}?") %>%
                       magrittr::set_names(c("i","x","i")) %>%
                       magrittr::extract(c(stringr::str_detect(cnd$message,
                        "line"), TRUE, stringr::str_detect(cnd$message,
                        "line")))), # add info only if applicable
                   call = call,
                   class = "error.meta.read.1")})

  yaml_input
}

meta.str.input <- function(x, arg = rlang::caller_arg(x),
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
                   class = "error.meta.str.input.1")})
}


meta.str.component <- function(x) {

  # Create empty list

  components.list <- c("options","var.list","na.codes","na.rules","mc.sets",
                       "dict","contras")


}

meta.str.options <- function(x, call = rlang::caller_env()) {

  # Create empty list

  options.list <- c("study.name",
                    "id.var", "id.pattern",
                    "read.from", "mc.handling", "double.entry",
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
                   class = "error.meta.str.options.1")})

}

meta.forgive.component.name <- function(x) {

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
