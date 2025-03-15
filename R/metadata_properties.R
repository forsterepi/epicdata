prop.id.var <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  }
)
