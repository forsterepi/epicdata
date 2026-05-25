#' Evaluate the state of the data
#'
#' Evaluates a set of characteristics of the dataset to be processed in order to
#' ensure that combining workflow steps by the user works as expeceted. The
#' state of the data is evaluated before and after a workflow step, i.e., at the
#' start and at the end of the corresponding function.
#'
#' @details
#' See workflow vignette for details
#'
#' @param d A data.frame, tibble, or data.table to be evaluated
#' @param metadata A S7 `epicdata::metadata` object
#' @param when A single string, either `before` or `after`, indicating if the
#' state of the data is evaluated at the start or end of a workflow step.
#' `before` will produce errors for users, while `after` will produce internal
#' errors for the developer.
#' @param all.vars.character TRUE or NULL (default). TRUE evaluates the
#' characteristic, while NULL skips evaluation. FALSE does not make sense as it
#' would result in incorrect errors for datasets that really consist only of
#' variables of type `text`. Thus, FALSE will be converted to NULL.
#'
#' @returns The input dataset invisibly. However, the function is called for its
#' side effects.
#' @export
#'
#' @examples
state.of.the.data <- function(d, metadata, when,
                              all.vars.character = NULL) {
  # Check inputs
  checkmate::assert_data_frame(d, null.ok = FALSE)

  checkmate::assert_string(when, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_choice(when, choices = c("before", "after"))

  S7::check_is_S7(metadata)
  checkmate::assert_class(metadata, "epicdata::metadata", null.ok = FALSE)

  checkmate::assert_flag(all.vars.character, na.ok = FALSE, null.ok = TRUE)

  # all.vars.character ----
  ## FALSE makes no sense for all.vars.character: FALSE -> NULL
  if (!all.vars.character) {
    all.vars.character <- NULL
  }

  if (!is.null(all.vars.character)) {
    ## Conduct check
    check.all.vars.character <- d %>%
      #purrr::map_lgl(isa, what = "character") %>%
      purrr::map_lgl(
        \(x) identical(vctrs::vec_ptype(x), character(0))
      ) %>%
      all()

    ## Evaluate check result
    if (check.all.vars.character != all.vars.character) {
      if (when == "before") {
        cli::cli_abort(
          "State of the data incorrect: All variables must be character!",
          class = "error.state.of.the.data.1"
        )
      }
      if (when == "after") {
        cli::cli_abort(c("x" = "Error IE21111"),
          .internal = TRUE, class = "IE21111", call = rlang::caller_env()
        )
      }
    }
  }


  invisible(d)
}
