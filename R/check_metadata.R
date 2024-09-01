#' Check `epic_metadata` class objects
#'
#' Check if `epic_metadata` class objects fulfill the conditions for being used
#' in data processing.
#'
#' @param metadata An `epic_metadata` object.
#'
#' @return Prints information about successful and unsuccessful checks in the
#'   console. Returns the input metadata. If checks were successful, returns
#'   checked metadata that can be used in data processing.
#' @export
#'
#' @examples
#' x <- empty_metadata()
#' check_metadata(x)
check_metadata <- function(metadata) {

  rlang::try_fetch(validate_metadata(metadata),
                   error = function(cnd) {
                     cli::cli_abort("{.var metadata} must be an object of class {.emph epic_metadata}!",
                                    parent = cnd, class = "no_epic_metadata")
                   })

  cli::cli_h1("Checking {.emph epic_metadata} {as.character(substitute(metadata))}")

  # metadata %<>% uncheck_metadata()
  #
  # rlang::try_fetch(check_does_keys(metadata),
  #                  error = function(e_cnd) {
  #                    cli::cli_inform(c("x" = "Checking DOES keywords failed!"), parent = e_cnd)
  #                  },
  #                  warning = function(w_cnd) {
  #                    cli::cli_inform(c("!" = "Checking DOES keywords resulted in warnings!"), parent = w_cnd)
  #                  })
  #
  # cli::cli_rule("Summary")
  #
  # rlang::try_fetch({
  #   spsUtil::quiet(check_does_keys(metadata))
  #   metadata %<>% unclass(.)
  #   metadata %<>% structure(., class = "epic_metadata_checked")
  #   cli::cli_alert_success("Checking successful!")
  # },
  # error = function(cnd) {
  #   cli::cli_alert_danger("Checking failed! Please correct errors and repeat.")
  # }
  # )

  invisible(metadata)
}
