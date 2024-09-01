#' Table shinymodule UI
#'
#' Shinymodule UI for tables using the DT package.
#'
#' @param id Namespace argument.
#'
#' @noRd
tableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(NS(id,"tbl"))
}

#' Table shinymodule server
#'
#' Shinymodule Server for tables using the DT package. It contains displaying
#' and editing tables.
#'
#' @param id Namespace argument.
#' @param metadata Metadata object, which contains the data as an
#'   `epic_metadata` class object. This argument is reactive.
#' @param metadata_part Addresses a specific data.frame element of metadata.
#'   Options are element names of class `epic_metadata`. This argument is
#'   not reactive.
#' @param editable A list of arguments defining how the table is editable. This
#'   argument is not reactive.
#' @param options A list of arguments defining additional options of the table.
#'   This argument is not reactive.
#'
#' @noRd
tableServer <- function(id, metadata, metadata_part, editable, options) {
  stopifnot(is.reactive(metadata))
  stopifnot(!is.reactive(metadata_part))
  stopifnot(!is.reactive(editable))
  stopifnot(!is.reactive(options))

  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDataTable(metadata()[[metadata_part]], server = T, rownames = F, selection = "none",
                                      editable = editable, options = options)

    observeEvent(input$tbl_cell_edit, {
      metadata_temp <- metadata()
      metadata_temp[[metadata_part]] <- DT::editData(metadata_temp[[metadata_part]], input$tbl_cell_edit, 'tbl', rownames = F)
      metadata(metadata_temp)
    })
  })
}

#' Table shinymodule demo app
#'
#' Demo app for shinymodule table, used for testing only.
#'
#' @noRd
tableApp <- function() {
  ui <- fluidPage(
    tableUI("mc_sets_tbl")
  )

  server <- function(input, output, session) {
    metadata <- reactiveVal(empty_metadata())
    tableServer("mc_sets_tbl", metadata, "mc_sets",
                editable = list(target = "cell", disable = list(columns = c(0))),
                options = list(scrollY = '500px', scrollCollapse = TRUE,
                               paging = FALSE, order = list(0,'asc'),
                               searchHighlight = TRUE))
  }

  shinyApp(ui, server)
}
