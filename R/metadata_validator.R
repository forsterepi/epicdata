metadata_validator <- function(self) {
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
