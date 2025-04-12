.onLoad <- function(...) {
  S7::methods_register()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

# Avoid the devtools::check() NOTE that the . used in piping is a non-declared
# variable
utils::globalVariables(".")

# devtools::check() does not realize that package withr is used in examples
# Strategy suggested in R packages (2e), 11.4.1.1 How to not use a package in
# Imports
ignore_unused_imports <- function() {
  withr::defer()
}

test.mode <- function(part) {
  if (exists(x = "test_mode", rlang::global_env())) {
    if (test_mode) {
      print(part)
      #print(as.numeric(Sys.time()), digits = 15)
    }
  }
}
