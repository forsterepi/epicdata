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


meta.str.component <- function() {

  # Create empty list

  components.list <- c("options","var.list","na.codes","na.rules","mc.sets",
                       "dict","contras")

  check.components.template <- vector(mode = "list",
                                   length = length(components.list)) %>%
    magrittr::set_names(components.list)

  # Define checks for every component

  check.components.template[["options"]] <- rlang::expr(
    {logger::log_info("Checking structure of options")
    meta.str.options(x)})
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

