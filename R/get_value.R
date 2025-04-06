get.value.touch.na <- function(meta, var) {

  # Check inputs

  val <- meta@var.list[[var]]$touch.na
  # gro ...
  # opt

  pre_defined_default <- TRUE

  if (!is.null(val)) {
    out <- val
  } else {
    if (!is.null(gro)) {
      out <- gro
    } else {
      if (!is.null(opt)) {
        out <- opt
      } else {
        out <- pre_defined_default
      }
    }
  }

  out
}
