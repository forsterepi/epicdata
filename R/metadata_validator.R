metadata_validator <- function(self) {
  if (!is.null(self@id.var)) {
    if (!(self@id.var %in% self@vars)) {
      "@id.var must be a variable specified in var.list"
    }
  }
}
