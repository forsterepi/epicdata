metadata.constructor <- function(file) {
  test.mode("metadata.constructor")

  # Static code check
  # TODO: Only continue if no errors are reported in the static check (check.yaml) (warnings and infos are fine)

  # Read in YAML file
  yaml_input <- yaml.read(file)

  # Check structure after reading-in the file
  yaml.str.input(yaml_input)
  yaml_input %<>% yaml.forgive.component.name()
  yaml.str.component(yaml_input)
  # yaml.str.options(yaml_input)
  yaml_input %<>% yaml.add.name() # Finalize and test this function
  # yaml.str.var.list(yaml_input)

  if (is.null(yaml_input$options$touch.na)) {
    touch.na.input <- yaml_input$options$na.touch
  } else {
    touch.na.input <- yaml_input$options$touch.na
  }

  # PUT INTO EXTRA FUNCTION
  for (i in seq_along(yaml_input$var.list)) {
    checkmate::assert_logical(yaml_input$var.list[[i]]$touch.na,
      len = 1, any.missing = FALSE, null.ok = TRUE
    )
    checkmate::assert_logical(yaml_input$var.list[[i]]$na.touch.,
      len = 1, any.missing = FALSE, null.ok = TRUE
    )
  }

  S7::new_object(S7::S7_object(),
    # Run the setter of var.list first
    var.list = yaml_input$var.list,
    # Run var.groups always after var.list
    var.groups = yaml_input$var.groups,
    # Global default options need to be listed after var.list and var.groups
    touch.na = touch.na.input,
    data.name = yaml_input$options$data.name,
    id.var = yaml_input$options$id.var,
    id.pattern = yaml_input$options$id.pattern,
    consent = yaml_input$options$consent
  )
}


# !!Add test!!


yaml.read <- function(x, arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {
  # Read
  rlang::try_fetch(
    {
      yaml_input <- yaml::read_yaml(x)
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Loading YAML metadata failed!",
          c(
            "Loading failed due to an error in the YAML grammar. The
                      {.code yaml::read_yaml()} error message below reports its
                      location. (Check lines before and after as well.)",
            cnd$message,
            "Check if you forgot any colons `:` or spaces after
                       colons `key: value`. Otherwise, try adding quotation
                       marks to keys and values with special characters."
          ) %>%
            magrittr::set_names(c("i", "x", "i")) %>%
            magrittr::extract(c(
              stringi::stri_detect(cnd$message, fixed = "line"),
              TRUE,
              stringi::stri_detect(cnd$message, fixed = "line")
            ))
        ), # add info only if applicable
        call = call,
        class = "error.yaml.read.1"
      )
    }
  )

  # Evaluate if spaces behind colons are missing


  yaml_input
}

yaml.str.input <- function(x, arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  # logger::log_info("Checking basic metadata input structure",
  #                  namespace = "epicdata")

  rlang::try_fetch(
    {
      # START: actual check
      checkmate::assert_list(x,
        min.len = 1, names = "unique", null.ok = FALSE,
        .var.name = arg
      )
      # END: actual check
    },
    error = function(cnd) {
      # logger::log_error("Checking basic metadata input structure failed",
      #                   namespace = "epicdata")
      # logger::log_debug(cnd$message, namespace = "epicdata")
      cli::cli_abort(
        c("YAML must describe a single list!",
          "x" = cnd$message,
          "i" = "Do not use dashes ({.var -}) in front of the
                            main metadata components: options, var.list, etc."
        ),
        call = call,
        class = "error.yaml.str.input.1"
      )
    }
  )
}

yaml.forgive.component.name <- function(x) {
  names(x) %<>% stringi::stri_trans_tolower()

  forgive.options <- c("option")
  forgive.var.list <- c(
    "varlist", "varslist", "vars.list", "list.var", "list.vars",
    "variable.list", "variables.list"
  )
  forgive.var.groups <- c("groups")
  forgive.na.codes <- c("codes.na")
  forgive.contras <- c("contra", "contradiction", "contradictions")

  names(x)[names(x) %in% forgive.options] <- "options"
  names(x)[names(x) %in% forgive.var.list] <- "var.list"
  names(x)[names(x) %in% forgive.var.groups] <- "var.groups"
  names(x)[names(x) %in% forgive.na.codes] <- "na.codes"
  names(x)[names(x) %in% forgive.contras] <- "contras"

  x
}

yaml.str.component <- function(x, arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  components.list <- c("options", "var.list", "var.groups", "na.codes", "contras")

  rlang::try_fetch(
    {
      # START: actual check
      checkmate::assert_subset(names(x), components.list, .var.name = arg)
      # END: actual check
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "YAML contains invalid component names!",
          cnd$message %>%
            stringi::stri_replace_all(
              replacement = "(",
              regex = "\\{"
            ) %>%
            stringi::stri_replace_all(
              replacement = ")",
              regex = "\\}"
            ),
          "Only the above listed components can be on the first
                     YAML layer, i.e., without indentation."
        ) %>%
          magrittr::set_names(c("!", "x", "i")),
        call = call,
        class = "error.yaml.str.component.1"
      )
    }
  )
}

yaml.str.options <- function(x, call = rlang::caller_env()) {
  # Create empty list

  options.list <- c(
    "data.name",
    "id.var", "id.pattern",
    "load.from", "mc.handling", "double.entry",
    "date.format", "format.date",
    "time.format", "format.time",
    "datetime.format", "format.datetime",
    "to.na", "touch.na",
    "to.factor",
    "translate"
  )

  check.options.template <- vector(
    mode = "list",
    length = length(options.list)
  ) %>%
    magrittr::set_names(options.list)


  # Define checks for every option

  check.options.template[["data.name"]] <- rlang::expr(
    checkmate::assert_string(x$options$data.name,
      min.chars = 1,
      na.ok = FALSE, null.ok = TRUE, add = coll,
      .var.name = "data.name"
    )
  )

  check.options.template[["id.var"]] <- rlang::expr(
    checkmate::assert_string(x$options$id.var,
      min.chars = 1,
      na.ok = FALSE, null.ok = TRUE, add = coll,
      .var.name = "id.var"
    )
  )

  check.options.template[["id.pattern"]] <- rlang::expr(
    checkmate::assert_string(x$options$id.pattern,
      min.chars = 1,
      na.ok = FALSE, null.ok = TRUE, add = coll,
      .var.name = "id.pattern"
    )
  )


  # Select

  options.used <- names(x$options)
  check.options <- check.options.template[options.used]


  # Eval

  rlang::try_fetch(
    {
      # START: actual check
      coll <- checkmate::makeAssertCollection()
      for (i in seq_along(check.options)) {
        eval(check.options[[i]])
      }
      checkmate::reportAssertions(coll)
      # END: actual check
    },
    error = function(cnd) {
      cli::cli_abort(
        c("Some options have not been specified correctly.",
          cnd$message %>% stringi::stri_split_regex("\n") %>%
            stringi::stri_trim_both() %>%
            magrittr::extract(2:length(.)) %>%
            stringi::stri_replace_all(
              replacement = "Option",
              regex = "^\\* Variable"
            ) %>%
            magrittr::set_names(rep("x", length(.))),
          "i" = "Some info"
        ),
        call = call,
        class = "error.yaml.str.options.1"
      )
    }
  )
}


yaml.str.var.list <- function(x, call = rlang::caller_env()) {
  # Check var.list when reading in YAML


  # dict.eng is not valid
}

yaml.str.var.groups <- function(x, call = rlang::caller_env()) {
  # Check var.groups when reading in YAML


  # dict.eng is not valid
}

yaml.str.na.codes <- function(x, call = rlang::caller_env()) {
  # Check var.groups when reading in YAML


  # dict.eng is not valid
}

yaml.str.contras <- function(x, call = rlang::caller_env()) {
  # Check var.groups when reading in YAML


  # dict.eng is not valid
}

yaml.add.name <- function(x) {
  for (i in seq_along(x$var.list)) {
    x$var.list[[i]]$var.name <- names(x$var.list)[i]
  }

  if (!is.null(x$var.groups)) {
    for (i in seq_along(x$var.groups)) {
      x$var.groups[[i]]$group.name <- names(x$var.groups)[i]
    }
  }

  x
}







### START Extraction test

# vars <- c("id", "hei", "var2", "f01")
# x <- "var2 !IN (one, two)& f01>21.02.2022 & 99 -> 9001"
#
# op <- "(!IN|IN|->|!=|==|>=|>|<=|<)" # substrings need to be to the right, e.g., IN must be to the right of !IN, as the evaluation is from lieft to right
#
# stringi::stri_extract_all_regex(x, op)

# count number of operators, i.e., how often each of them appears
# sum should be exactly 1, i.e., one operator appears once and all others dont appear at all

# same for variables (here overlap might be possible as well with less control :/)

# x %>% stringi::stri_split("&") %>% stringi::stri_trim_both())



### END Extraction test


# creating new variables with mutate
# var_name <- "log_var3"
# new_value <- "log(var3) + 5"
# x <- data.frame(var3 = c(1:5))
# x %>% dplyr::mutate("{var_name}" := eval(parse(text = new_value)))
# mutate all variables with new: field from top to bottom
# before re-arrange var.list in case newly created variables are used to create other variables
# values not allowes in new:? NULL, across(
# to test new: expressions, create a fake dataset with the variables and corresponding types and values

# handling format for date, time, datetime:
# - if there are individual format specifications in var.list, variable format will be created
# - if there are no individual format specifications in var.list, it needs to be created
# - fill in the empty cells, where a corresponding format is needed with the values from date.format, etc.
