#' Missing Codes shinymodule UI
#'
#' Shinymodule UI for the Missing Codes tab.
#'
#' @param id Namespace argument.
#'
#' @noRd
missing_codesUI <- function(id) {
  ns <- NS(id)
  tabPanel("Missing Codes",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h3("Missing Codes Entries"),
                          selectInput(NS(id,"input_class"), label = "Code class", choices = c("MISSING", "JUMP"), width = "250px") %>%
                            prompter::add_prompt(position = "right", message = "Is it a Jump Code or Missing Code?"),
                          br(),
                          numericInput(NS(id,"input_id"), label = "Missing/Jump Code", value = 9000, min = 1000, max = 9999) %>%
                            prompter::add_prompt(position = "right", message = "A 4-digit code is recommended, 8001,... for JUMP and 9001,... for MISSING codes."),
                          br(),
                          textInput(NS(id,"input_label"), label = "Code label", placeholder = "Label") %>%
                            prompter::add_prompt(position = "right", message = "Add the missing/jump code's label."),
                          br(),
                          actionButton(NS(id,"add"), label = "Add Missing/Jump Code") %>%
                            prompter::add_prompt(position = "right",
                                                 message = "Fill in the fields above and click the button to add a missing_codesionary entry. IDs are added automatically."),
                          br(),br(),
                          actionButton(NS(id,"delete"), label = "Delete row") %>%
                            prompter::add_prompt(position = "right", message = "Delete a row from the table by specifying the corresponding ID in the pop-up window."),
             ),
             mainPanel(width = 9,
                       tableUI(NS(id,"tbl"))
             )
           )
  )
}

#' Missing Codes shinymodule server
#'
#' Shinymodule Server for the Missing Codes tab.
#'
#' @param id Namespace argument.
#' @param metadata Metadata object, which contains the data as an
#'   `epic_metadata` class object. This argument is reactive.
#'
#' @noRd
missing_codesServer <- function(id, metadata) {
  stopifnot(is.reactive(metadata))
  metadata_part <- "missing_codes"

  # moduleServer
  moduleServer(id, function(input, output, session) {

    tableServer("tbl", metadata, metadata_part,
                editable = list(target = "cell", disable = list(columns = c(0,2))),
                options = list(scrollY = '750px', scrollCollapse = TRUE,
                               paging = FALSE, searchHighlight = TRUE))

    observeEvent(input$delete, {
      shinyalert::shinyalert(title = "Delete row", type = "input", inputType = "text",
                             inputPlaceholder = paste0("Provide id_",metadata_part),
                             callbackR = function(delete_id) {
                               metadata_temp <- metadata()
                              metadata_temp[[metadata_part]] %<>%
                                dplyr::filter(.data[[paste0("id_",metadata_part)]] != delete_id & .data[[paste0("id_",metadata_part)]] != paste0("01.01.",delete_id))
                              metadata(metadata_temp)}
      )
    })

    observeEvent(input$input_class, {
      if (input$input_class == "MISSING") {
        updateNumericInput(session, inputId = "input_id", label = "Missing/Jump Code", value = 9000, min = 1000, max = 9999)
      } else {
        updateNumericInput(session, inputId = "input_id", label = "Missing/Jump Code", value = 8000, min = 1000, max = 9999)
      }
    })

    observeEvent(input$add, {
      metadata_temp <- metadata()

      if (input$input_id %>% as.character() %>% magrittr::is_in(metadata_temp[[metadata_part]][["id_missing_codes"]])) {
        shinyalert::shinyalert(title = "Warning!", text = "This missing/jump code already exists.", type = "warning")
        updateTextInput(session, inputId = "input_label", label = "Code label", value = "")
        if (input$input_class == "MISSING") {
          updateNumericInput(session, inputId = "input_id", label = "Missing/Jump Code", value = 9000, min = 1000, max = 9999)
        } else {
          updateNumericInput(session, inputId = "input_id", label = "Missing/Jump Code", value = 8000, min = 1000, max = 9999)
        }
      } else {
        to_add <- data.frame(id_missing_codes = c(input$input_id %>% as.character(), paste0("01.01.",input$input_id)),
                             label_missing_codes = rep(input$input_label %>% stringr::str_trim("both"),2),
                             class_missing_codes = rep(input$input_class,2))

        metadata_temp[[metadata_part]] <- rbind(metadata_temp[[metadata_part]],to_add)
        metadata(metadata_temp)

        updateTextInput(session, inputId = "input_label", label = "Code label", value = "")
        if (input$input_class == "MISSING") {
          updateNumericInput(session, inputId = "input_id", label = "Missing/Jump Code", value = 9000, min = 1000, max = 9999)
        } else {
          updateNumericInput(session, inputId = "input_id", label = "Missing/Jump Code", value = 8000, min = 1000, max = 9999)
        }
      }
    })
  })
}

#' Missing Codes shinymodule demo app
#'
#' Demo app for the shinymodule of the Missing Codes tab, used for testing only.
#'
#' @noRd
missing_codesApp <- function() {
  ui <- tagList(
    prompter::use_prompt(),
    navbarPage(
      title = "Create Steps for SCC Models",
      missing_codesUI("missing_codes_tab")
    )
  )

  server <- function(input, output, session) {
    metadata <- reactiveVal(empty_metadata())

    missing_codesServer("missing_codes_tab", metadata)

  }

  shinyApp(ui, server)
}
