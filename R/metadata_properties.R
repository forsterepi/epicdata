# options

meta.prop.study.name <- S7::new_property(
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

meta.prop.id.var <- S7::new_property(
  class = S7::class_character | NULL,
  validator = function(value) {
    if (!(is.null(value) | length(value) == 1L)) {
      "must have length 1"
    }
  }
)

# Modules

meta.prop.DUP_NO.ID <- S7::new_property(
  getter = function(self) {
    if (is.null(self@id.var)) {
      TRUE
    } else {
      FALSE
    }
  }
)

meta.prop.DUP_FREQ <- S7::new_property(
  getter = function(self) {
    if (is.null(self@id.var)) {
      FALSE
    } else {
      TRUE
    }
  }
)
