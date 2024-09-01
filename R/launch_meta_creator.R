#' Launch meta creator `shiny` app
#'
#' @return The `launch_meta_creator` function is used for the side effect of starting the Meta Data Creator `shiny` app.
#' @export
#'
#' @examples
#' \dontrun{
#' launch_meta_creator()
#' }
launch_meta_creator <- function() {
  appDir <- system.file("shiny", "meta_creator", package = "epicdata")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `epicdata`.", call. = FALSE)
  }

  #shiny::runApp(appDir, display.mode = "normal")
  shiny::shinyAppDir(appDir)
}
