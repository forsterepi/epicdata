metadata.validator <- function(self) {
  if (!is.null(self@id.var)) {
    if (!(self@id.var %in% self@var.names)) {
      return("@id.var must be a variable specified in var.list")
    }
  }
  if (!is.null(self@touch.na)) {
    check <- rep(NA, length(self@var.list))
    for (i in seq_along(self@var.list)) {
      check[i] <- self@var.list[[i]]$touch.na.default.option
    }
    if (self@touch.na %>% magrittr::equals(check) %>% magrittr::not() %>%
        any()) {
      which_var <- names(self@var.list)[self@touch.na %>%
                                          magrittr::equals(check) %>%
                                          magrittr::not()]
      return(paste0("Do not change @var.list$",
             which_var,
             "$touch.na.default.option directly. Change @touch.na instead."))
    }
  }
}


# group.names must be unique

var.list.validator <- function(value) {
  # Check basic list structure
  if (!checkmate::test_list(value, any.missing = FALSE, min.len = 1,
                           names = "unique", null.ok = FALSE)) {
    return("must have unique variable names, must have at least 1 variable,
           must not contain any NAs, and must not be NULL.")
  }
  # Loop over all variables
  for (i in seq_along(value)) {
    if (!checkmate::test_subset(names(value[[i]]), empty.ok = FALSE,
                     choices = c("var.name","type","old.id","label",
                     "label.eng","to.na","touch.na","na.else","na.rules",
                     "na.switch","na.keep","na.replace","group","new",
                     "ops","dict","cats","cats.eng","to.factor","factor.name",
                     "name.factor","from","from.exclude","to","to.exclude",
                     "format","date.format","format.date","time.format",
                     "format.time","datetime.format","format.datetime",
                     "to.na.default.group","touch.na.default.group",
                     "na.else.default.group","na.rules.default.group",
                     "na.switch.default.group","na.keep.default.group",
                     "na.replace.default.group","dict.default.group",
                     "cats.default.group","cats.eng.default.group",
                     "to.factor.default.group","from.default.group",
                     "from.exclude.default.group","to.default.group",
                     "to.exclude.default.group","format.default.group",
                     "date.format.default.group","format.date.default.group",
                     "time.format.default.group","format.time.default.group",
                     "datetime.format.default.group",
                     "format.datetime.default.group",
                     "to.na.default.option","touch.na.default.option",
                     "to.factor.default.option","date.format.default.option",
                     "format.date.default.option","time.format.default.option",
                     "format.time.default.option",
                     "datetime.format.default.option",
                     "format.datetime.default.option"
                     ))) {
      return("contains invalid keys")
    }
  }

  # Check that var.names are syntactically valid names
  # if (make.names(names(value)) %>% magrittr::equals(names(value)) %>% all()) {
  #   return("Variables must have syntactically valid names")
  # }

}

var.groups.validator <- function(value) {

}
