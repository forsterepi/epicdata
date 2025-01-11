#' Check `epicdata_meta` class objects
#'
#' Check if `epicdata_meta` class objects fulfill the conditions for being used
#' in data processing.
#'
#' @param metadata An `epicdata_meta` object.
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
                     cli::cli_abort("{.var metadata} must be an object of class {.emph epicdata_meta}!",
                                    parent = cnd, class = "no_epicdata_meta")
                   })

  cli::cli_h1("Checking {.emph epicdata_meta} {as.character(substitute(metadata))}")

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
  #   metadata %<>% structure(., class = "epicdata_meta_checked")
  #   cli::cli_alert_success("Checking successful!")
  # },
  # error = function(cnd) {
  #   cli::cli_alert_danger("Checking failed! Please correct errors and repeat.")
  # }
  # )

  invisible(metadata)
}
