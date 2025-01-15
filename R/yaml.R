read_meta <- function(file) {
  # Check input


  #=============================================================================
  # Read in YAML file
  x <- yaml::read_yaml(file)

  # Check basic structure after reading-in the file


  # Process read-in file

  ## Process var.list fields cats and cats.eng
  x %<>% yaml.prc.var.list.cats("cats")
  x %<>% yaml.prc.var.list.cats("cats.eng")

  # Transform corresponding elements to data.frames
  x %<>% purrr::map(\(x) if (is.list(x)) {
    purrr::map_dfr(x, as.data.frame)
  } else {
    as.character(x)
  })

  # Transform to epicdata_meta
  x %<>% new_metadata()

  # Validate
  x %<>% validate_metadata()

  return(x)
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
  options.list <- c("study.name", "id.var", "read.from",
                    "date.format", "format.date",
                    "time.format", "format.time",
                    "datetime.format", "format.datetime",
                    "mc.handling", "double.entry", "translate", "id.pattern")
  options.read.from <- c("folder", "db")
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
var_name <- "log_var3"
new_value <- "log(var3) + 5"
x <- data.frame(var3 = c(1:5))
x %>% dplyr::mutate("{var_name}" := eval(parse(text = new_value)))
# mutate all variables with new: field from top to bottom
# before re-arrange var.list in case newly created variables are used to create other variables
# values not allowes in new:? NULL, across(
# to test new: expressions, create a fake dataset with the variables and corresponding types and values

# handling format for date, time, datetime:
# - if there are individual format specifications in var.list, variable format will be created
# - if there are no individual format specifications in var.list, it needs to be created
# - fill in the empty cells, where a corresponding format is needed with the values from date.format, etc.
