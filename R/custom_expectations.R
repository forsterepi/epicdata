expect_metadata <- function(object, meta) {
  # Capture object and label
  o <- testthat::quasi_label(rlang::enquo(object))

  # Fail

  # Pass

  # Invisibly return the input value
  invisible(o$val)
}
