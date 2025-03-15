# options

prop.study.name <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    if (!(length(value) == 1L)) {
      "must have length 1"
    }
  },
  setter = function(self, value) {
    if (is.null(value)) {
      self@study.name <- "my_study"
    } else {
      self@study.name <- value
    }
    self
  }
)

prop.id.var <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  }
)
