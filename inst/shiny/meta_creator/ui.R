#' UI of Metadata Creator `shiny` app
#'
#' The UI of Metadata Creator `shiny` app including UI modules and options.
#'
#' @noRd
ui <- tagList(
  shinyjs::useShinyjs(),
  prompter::use_prompt(),
  tags$style(type = "text/css", ".form-control.shiny-bound-input, .numeric-input {height: 35px;}"),
  navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    title = "Create Metadata",
    ui_start_tab(),
    missing_codesUI("missing_codes_tab"),
    ui_main_tab(),
    ui_missing_rules_tab(),
    dictUI("dict_tab"),
    ui_help_tab()
  )
)
