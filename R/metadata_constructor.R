metadata.constructor <- function(file) {
  test.mode("metadata.constructor")

  # Check file exists
  rlang::try_fetch(
    checkmate::assert_file_exists(
      file,
      access = "r",
      extension = c("yml", "yaml")
    ),
    error = function(cnd) {
      cli::cli_abort(
        c("x" = "Error!", "!" = cnd$message),
        class = "error.metadata.constructor.1",
        parent = NA
      )
    }
  )

  # Read yaml
  rlang::try_fetch(
    {
      yaml_input <- yaml12::read_yaml(file)

      # na.codes as key names (as.character)
      if (!is.null(yaml_input$na.codes)) {
        if (!is.null(yaml_input$na.codes |> attr("yaml_keys"))) {
          names(yaml_input$na.codes) <- yaml_input$na.codes |>
            attr("yaml_keys") |>
            unlist() |>
            as.character()
        }
      }
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Loading YAML metadata failed!",
          c(
            paste0(
              "Loading failed due to an error in the YAML grammar.",
              "The {.code yaml12::read_yaml()} error message below reports its location.",
              "(Check lines before and after as well.)"
            ),
            cnd$message,
            paste0(
              "Check if you forgot any colons `:` or spaces after colons `key: value`.",
              "Otherwise, try adding quotation marks to keys and values with special characters."
            )
          ) |>
            magrittr::set_names(c("i", "x", "i")) |>
            magrittr::extract(c(
              stringi::stri_detect(cnd$message, fixed = "line"),
              TRUE,
              stringi::stri_detect(cnd$message, fixed = "line")
            ))
        ), # add info only if applicable
        class = "error.metadata.constructor.2",
        parent = NA
      )
    }
  )

  # Check structure with JSON schema
  ## Load schema
  schema_path <- system.file("epicdata-schema.json", package = "epicdata")

  ## Create validator
  v <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")

  ## Validate
  res <- yaml_input |>
    jsonlite::toJSON(auto_unbox = TRUE, null = "null") |>
    v$validate(verbose = TRUE) |>
    attributes() |>
    magrittr::use_series("error")

  ## Report results
  if (!is.null(res)) {
    ## Extract information from validation results
    res <- res |>
      tidyr::hoist(schema, "required", .transform = \(x) {
        stringi::stri_c(x, collapse = " & ")
      }) |>
      tidyr::hoist(data, "type") |>
      dplyr::mutate(
        schemaPath = schemaPath |>
          stringi::stri_replace_all_regex(
            "/patternProperties/[^/]*/",
            "/patternProperties/"
          ) |>
          stringi::stri_replace_all_regex(
            "/allOf/[0-9]*/not$",
            "/allOf/not"
          ),
        pattern_prop = stringi::stri_detect_fixed(
          schemaPath,
          "patternProperties"
        ),
        key = dplyr::case_when(
          pattern_prop ~ instancePath |>
            stringi::stri_split_fixed("/") |>
            purrr::map(magrittr::extract, 3) |>
            unlist()
        ),
        component = dplyr::case_when(
          stringi::stri_detect_regex(
            str = schemaPath,
            pattern = "^#/properties/"
          ) ~ instancePath |>
            stringi::stri_split_fixed("/") |>
            purrr::map(magrittr::extract, 2) |>
            unlist()
        ),
        invalid_prop = dplyr::case_when(
          keyword == "additionalProperties" ~ params[["additionalProperty"]],
          keyword == "unevaluatedProperties" ~ params[["unevaluatedProperty"]]
        ),
        alias = dplyr::case_when(
          required != "" ~ required,
          required == "" ~ NA
        )
      ) |>
      dplyr::select(schemaPath, key, component, invalid_prop, type, alias) |>
      dplyr::left_join(
        better.json.validate.error.messages,
        by = "schemaPath"
      ) |>
      dplyr::mutate(
        schemaPath = dplyr::case_when(
          schemaPath ==
            "#/properties/var.list/patternProperties/unevaluatedProperties" ~ paste0(
            schemaPath,
            "/",
            key
          ),
          schemaPath ==
            "#/properties/var.groups/patternProperties/additionalProperties" ~ paste0(
            schemaPath,
            "/",
            key
          ),
          schemaPath ==
            "#/properties/import/patternProperties/additionalProperties" ~ paste0(
            schemaPath,
            "/",
            key
          ),
          .default = schemaPath
        )
      ) |>
      dplyr::summarise(
        insert_keys = stringi::stri_c(key, collapse = ", "),
        insert_component = dplyr::first(component),
        insert_invalid_props = stringi::stri_c(invalid_prop, collapse = ", "),
        insert_type = dplyr::first(type),
        insert_aliases = stringi::stri_c(alias, collapse = ", "),
        my_error = dplyr::first(my_error),
        vignette_hint = dplyr::first(vignette_hint),
        names_hint = dplyr::first(names_hint),
        boolean_hint = dplyr::first(boolean_hint),
        .by = schemaPath
      ) |>
      dplyr::mutate(
        insert_keys = insert_keys |>
          stringi::stri_split_fixed(", ") |>
          purrr::map(\(x) x |> unique() |> stringi::stri_c(collapse = ", ")) |>
          unlist(),
        my_error = my_error |>
          stringi::stri_replace_all_fixed(
            pattern = "{insert_keys}",
            replacement = insert_keys
          ) |>
          stringi::stri_replace_all_fixed(
            pattern = "{insert_component}",
            replacement = insert_component
          ) |>
          stringi::stri_replace_all_fixed(
            pattern = "{insert_invalid_props}",
            replacement = insert_invalid_props
          ) |>
          stringi::stri_replace_all_fixed(
            pattern = "{insert_type}",
            replacement = insert_type
          ) |>
          stringi::stri_replace_all_fixed(
            pattern = "{insert_aliases}",
            replacement = insert_aliases
          )
      )

    ## Prepare messages for cli_abort()
    res$error_number <- seq_len(nrow(res))
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
      class = "error.metadata.constructor.3",
      parent = NA
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
    "TRUE",
    "FALSE",
    "FALSE",

    "#/required",
    "The metadata specification must at least contain {.strong var.list}.",
    "TRUE",
    "FALSE",
    "FALSE",

    "#/additionalProperties",
    paste0(
      "The first level can only contain keys {.strong options}, ",
      "{.strong var.list}, {.strong var.groups}, {.strong na.codes}, ",
      "{.strong contras}, and {.strong inport}."
    ),
    "TRUE",
    "FALSE",
    "FALSE",

    ###
    # options
    ###

    "#/properties/options/type",
    paste0(
      "Key `options` must have options specified below it.",
      "These options must be in a new line and indented by two spaces."
    ),
    "FALSE",
    "FALSE",
    "FALSE",

    "#/properties/options/properties/data.name/type",
    "Option `data.name` must be a string.",
    "FALSE",
    "FALSE",
    "FALSE",

    "#/properties/options/properties/id.var/pattern",
    "Option `id.var` must contain a valid variable name.",
    "FALSE",
    "TRUE",
    "FALSE",

    "#/properties/options/properties/id.var/type",
    "Option `id.var` must contain a valid variable name.",
    "FALSE",
    "TRUE",
    "FALSE",

    "#/properties/options/properties/consent/type",
    "Option `consent` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/options/properties/id.list/type",
    "Option `id.list` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/options/properties/na.touch/type",
    "Option `na.touch` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/options/properties/touch.na/type",
    "Option `touch.na` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/options/properties/remove.vars/type",
    "Option `remove.vars` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/options/properties/vars.remove/type",
    "Option `vars.remove` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    ###
    # var.list
    ###

    "#/properties/var.list/type",
    paste0(
      "`var.list` must have at least one variable specified.",
      "Variables must be in a new line and indented by two spaces.",
      "Variable-specific keys must again be in a new line and indented."
    ),
    "TRUE",
    "FALSE",
    "FALSE",

    "#/properties/var.list/additionalProperties",
    "In `var.list`, variable/s {.strong {insert_invalid_props}} has/have invalid names.",
    "TRUE",
    "FALSE",
    "FALSE",

    "#/properties/var.list/patternProperties/unevaluatedProperties",
    "All variable names must be syntactically valid.",
    "FALSE",
    "TRUE",
    "FALSE",

    "#/properties/var.list/patternProperties/properties/type/type",
    paste0(
      "Key `type` must be one of `text`, `num`, `cat`, `date`, ",
      "`datetime`, or `time.`"
    ),
    "TRUE",
    "FALSE",
    "FALSE",

    "#/properties/var.list/patternProperties/properties/type/enum",
    paste0(
      "Key `type` must be one of `text`, `num`, `cat`, `date`, ",
      "`datetime`, or `time.`"
    ),
    "TRUE",
    "FALSE",
    "FALSE",

    "#/properties/var.list/patternProperties/properties/label/type",
    "Key `label` must be a string.",
    "FALSE",
    "FALSE",
    "FALSE",

    "#/properties/var.list/patternProperties/properties/group/pattern",
    "Key `group` must contain a valid variable name.",
    "FALSE",
    "TRUE",
    "FALSE",

    "#/properties/var.list/patternProperties/properties/group/type",
    "Key `group` must contain a valid variable name.",
    "FALSE",
    "TRUE",
    "FALSE",

    "#/properties/var.list/patternProperties/properties/na.touch/type",
    "Key `na.touch` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/var.list/patternProperties/properties/touch.na/type",
    "Key `touch.na` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/var.list/patternProperties/allOf/not",
    paste0(
      "In `var.list`, for variable/s {.strong {insert_keys}} multiple versions ",
      "of the same key have been specified, namely for: {.strong {insert_aliases}}."
    ),
    "TRUE",
    "FALSE",
    "FALSE",

    ###
    # var.groups
    ###

    "#/properties/var.groups/patternProperties/properties/na.touch/type",
    "Key `na.touch` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    "#/properties/var.groups/patternProperties/properties/touch.na/type",
    "Key `touch.na` must be `true` or `false`.",
    "FALSE",
    "FALSE",
    "TRUE",

    ###
    # na.codes
    ###

    "#/properties/na.codes/patternProperties/type",
    "For NA codes, after the colon, define the missingness type with text.",
    "TRUE",
    "FALSE",
    "FALSE",

    ###
    # import
    ###
    "#/properties/import/additionalProperties",
    "All imported datasets must have syntactically valid names.",
    "FALSE",
    "TRUE",
    "FALSE",

    "#/properties/import/patternProperties/required",
    "All imported datasets need keys `id` and `vars`.",
    "TRUE",
    "FALSE",
    "FALSE",

    "#/properties/import/patternProperties/additionalProperties",
    "For imported datasets, only keys `id` and `var` are allowed.",
    "TRUE",
    "FALSE",
    "FALSE"
  ),
  ncol = 5,
  byrow = TRUE
) %>%
  as.data.frame() %>%
  magrittr::set_colnames(c(
    "schemaPath",
    "my_error",
    "vignette_hint",
    "names_hint",
    "boolean_hint"
  )) |>
  dplyr::mutate(
    dplyr::across(!c(schemaPath, my_error), as.logical)
  )

better.json.validate.error.messages.hints <- c(
  vignette_hint = "Check {.vignette epicdata::metadata_long} for more information.",
  names_hint = "Look at `?make.names` for details.",
  boolean_hint = "Please don't use `yes`, `no`, `on`, `off`, `y`, or `n`."
)

# #
# #
# #
# #
# #

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
