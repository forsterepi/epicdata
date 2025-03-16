#' @include variable_properties.R
NULL

#' Variable objects
#'
#' S7 class `epicmodel::variable`
#'
#' @param name The name of the variable
#' @param type x
#' @param old.id x
#' @param label x
#' @param label.eng x
#' @param cats x
#' @param cats.eng x
#'
#' @returns An S7 `epicdata::variable` object
#'
variable <- S7::new_class("variable",
  properties = list(
    name = var.prop.name,
    type = var.prop.type,
    old.id = var.prop.old.id,
    label = var.prop.label,
    label.eng = var.prop.label,
    cats = var.prop.cats,
    cats.eng = var.prop.cats.eng
  ),
  validator = function(self) {
    # old.id
    if (!is.null(self@old.id)) {
      if (self@name == self@old.id) {
        "@old.id must not be the same as @name"
      }
    # label & label.eng
    } else if (!is.null(self@label.eng)) {
      if (is.null(self@label)) {
        "@label.eng can only be used together with @label"
      }
    # cats & cats.eng
    } else if (self@type != "cat" & !is.null(self@cats)) {
      "@cats can only be used for variables of @type 'cat'"
    } else if (self@type != "cat" & !is.null(self@cats.eng)) {
      "@cats.eng can only be used for variables of @type 'cat'"
    } else if (!is.null(self@cats.eng)) {
      if (is.null(self@cats)) {
        "@cats.eng can only be used together with @cats"
      } else if (length(self@cats$level) != length(self@cats.eng$level)) {
        "@cats.eng must have levels identical to @cats"
      } else if (!all(self@cats$level == self@cats.eng$level)) {
        "@cats.eng must have levels identical to @cats"
      }
    }
  }
)
