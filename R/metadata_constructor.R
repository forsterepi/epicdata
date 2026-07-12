metadata.constructor <- function(file) {
  test.mode("metadata.constructor")

  # Check file exists
  rlang::try_fetch(
    {
      checkmate::assert_file_exists(
        file,
        access = "r",
        extension = c("yml", "yaml")
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        cnd$message,
        call = rlang::caller_env(),
        class = "error.metadata.constructor.1"
      )
    }
  )

  # Read yaml
  rlang::try_fetch(
    {
      yaml_input <- yaml12::read_yaml(file)
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Loading YAML metadata failed!",
          c(
            "Loading failed due to an error in the YAML grammar. The {.code yaml12::read_yaml()} error message below reports its location. (Check lines before and after as well.)",
            cnd$message,
            "Check if you forgot any colons `:` or spaces after colons `key: value`. Otherwise, try adding quotation marks to keys and values with special characters."
          ) %>%
            magrittr::set_names(c("i", "x", "i")) %>%
            magrittr::extract(c(
              stringi::stri_detect(cnd$message, fixed = "line"),
              TRUE,
              stringi::stri_detect(cnd$message, fixed = "line")
            ))
        ), # add info only if applicable
        call = rlang::caller_env(),
        class = "error.metadata.constructor.2"
      )
    }
  )

  # Check structure with JSON schema
  ## Load schema
  schema_path <- system.file("epicdata-schema.json", package = "epicdata")

  ## Create validator
  v <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")

  ## Validate
  res <- yaml_input %>%
    jsonlite::toJSON(auto_unbox = TRUE, null = "null") %>%
    v$validate(verbose = TRUE) %>%
    attributes() %>%
    magrittr::use_series("error")

  ## Report results
  if (!is.null(res)) {
    ## Merge good error messages
    res %<>%
      dplyr::left_join(
        better.json.validate.error.messages,
        by = "schemaPath"
      ) %>%
      dplyr::select(my_error, my_hint)

    ## Prepare messages for cli_abort()
    res$error_number <- 1:nrow(res)
    res_error <- res %>%
      dplyr::select(error_number, message = my_error) %>%
      dplyr::mutate(
        type = "x"
      )
    res_hint <- res %>%
      dplyr::select(error_number, message = my_hint) %>%
      dplyr::mutate(
        type = "i"
      ) %>%
      dplyr::filter(message != "")
    res <- rbind(
      data.frame(
        error_number = 0L,
        message = "The YAML input has an incorrect structure.",
        type = "x"
      ),
      res_error,
      res_hint
    ) %>%
      dplyr::arrange(error_number, dplyr::desc(type))

    number_of_errors <- max(res$error_number)
    ### Only report up to 5 errors
    if (number_of_errors > 5) {
      res %<>%
        filter(error_number <= 5)
    }

    errors <- res$message %>%
      magrittr::set_names(res$type)

    if (number_of_errors == 6) {
      errors <- c(errors, "... and 1 more problem")
    }
    if (number_of_errors > 6) {
      errors <- c(
        errors,
        paste0("... and ", number_of_errors - 5, " more problems")
      )
    }

    cli::cli_abort(
      errors,
      call = rlang::caller_env(),
      class = "error.metadata.constructor.2"
    )
  }

  # Add the variable name as var.list to the list of elements for this variable
  for (i in seq_along(yaml_input$var.list)) {
    yaml_input$var.list[[i]]$var.name <- names(yaml_input$var.list)[i]
  }

  # Add the group name as group.name to the list of elements for this group
  if (!is.null(yaml_input$var.groups)) {
    for (i in seq_along(yaml_input$var.groups)) {
      yaml_input$var.groups[[i]]$group.name <- names(yaml_input$var.groups)[i]
    }
  }

  # Aliases
  if (is.null(yaml_input$options$touch.na)) {
    touch.na.input <- yaml_input$options$na.touch
  } else {
    touch.na.input <- yaml_input$options$touch.na
  }

  if (is.null(yaml_input$options$remove.vars)) {
    remove.vars.input <- yaml_input$options$vars.remove
  } else {
    remove.vars.input <- yaml_input$options$remove.vars
  }

  # Create S7 object
  S7::new_object(
    S7::S7_object(),
    ## Run the setter of var.list first
    var.list = yaml_input$var.list,
    ## Run var.groups always after var.list
    var.groups = yaml_input$var.groups,
    ## Global default options need to be listed after var.list and var.groups
    touch.na = touch.na.input,
    data.name = yaml_input$options$data.name,
    id.var = yaml_input$options$id.var,
    id.pattern = yaml_input$options$id.pattern,
    consent = yaml_input$options$consent,
    remove.vars = remove.vars.input
  )
}

# Data.frame translating error messages from JSON schema validation ----
better.json.validate.error.messages <- matrix(
  c(
    "#/type",
    "The metadata specification must at least contain a variable list.",
    "Check {.vignette epicdata::metadata_long} for more information.",

    "#/required",
    "The metadata specification must at least contain a variable list.",
    "Check {.vignette epicdata::metadata_long} for more information.",

    ###
    # options
    ###

    "#/properties/options/properties/id.var/pattern",
    "Option `id.var` must contain a valid variable name.",
    "Look at `?make.names` for details.",

    "#/properties/options/properties/id.var/type",
    "Option `id.var` must contain a valid variable name.",
    "Look at `?make.names` for details.",

    "#/properties/options/properties/consent/type",
    "Option `consent` must be `true` or `false`.",
    "Please don't use `yes`, `no`, `on`, `off`, `y`, or `n`.",

    ###
    # var.list
    ###

    "#/properties/var.list/type",
    "`var.list` must have at least one variable specified.",
    "Check {.vignette epicdata::metadata_long} for more information.",

    "#/properties/var.list/additionalProperties",
    "All variable names must be syntactically valid.",
    "Look at `?make.names` for details."
  ),
  ncol = 3,
  byrow = TRUE
) %>%
  as.data.frame() %>%
  magrittr::set_colnames(c("schemaPath", "my_error", "my_hint"))


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
