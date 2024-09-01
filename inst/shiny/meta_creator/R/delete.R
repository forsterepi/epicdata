#' Delete shinymodule UI
#'
#' Shinymodule UI for the delete actionButton.
#'
#' @param id Namespace argument.
#'
#' @noRd
deleteUI <- function(id) {
  ns <- NS(id)
  actionButton(NS(id,"delete"), label = "Delete row") %>%
    prompter::add_prompt(position = "right", message = "Delete a row from the table by specifying the corresponding ID in the pop-up window.")
}

#' Delete shinymodule server
#'
#' Shinymodule Server for the delete actionButton.
#'
#' @param id Namespace argument.
#' @param metadata Metadata object, which contains the data as an
#'   `epic_metadata` class object. This argument is reactive.
#' @param metadata_part Addresses a specific data.frame element of metadata.
#'   Options are element names of class `epic_metadata`. This argument is
#'   not reactive.
#'
#' @noRd
deleteServer <- function(id, metadata, metadata_part) {
  stopifnot(is.reactive(metadata))
  stopifnot(!is.reactive(metadata_part))

  moduleServer(id, function(input, output, session) {
    observeEvent(input$delete, {
      shinyalert::shinyalert(title = "Delete row", type = "input", inputType = "text",
                             inputPlaceholder = paste0("Provide id_",metadata_part),
                 callbackR = function(delete_id) {
                   metadata_temp <- metadata()
                   metadata_temp[[metadata_part]] %<>% dplyr::filter(.data[[paste0("id_",metadata_part)]] != delete_id)
                   metadata(metadata_temp)}
      )
    })
  })
}

#' Delete shinymodule demo app
#'
#' Demo app for shinymodule delete, used for testing only.
#'
#' @noRd
deleteApp <- function() {

  ui <- fluidPage(
    prompter::use_prompt(),
    tableUI("mc_sets_tbl"),
    deleteUI("mc_sets_delete")
  )

  server <- function(input, output, session) {
    metadata <- reactiveVal(empty_metadata())

    tableServer("mc_sets_tbl", metadata, "mc_sets",
                editable = list(target = "cell", disable = list(columns = c(0))),
                options = list(scrollY = '500px', scrollCollapse = TRUE,
                               paging = FALSE, order = list(0,'asc'),
                               searchHighlight = TRUE))

    deleteServer("mc_sets_delete", metadata, "mc_sets")
  }

  shinyApp(ui, server)
}
