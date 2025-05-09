#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
#' @noRd
NULL

#' Assignment pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%<>\%}} for details.
#'
#' @name %<>%
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @param lhs An object which serves both as the initial value and as target.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `lhs <- rhs(lhs)`.
#' @noRd
NULL
