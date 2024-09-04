#' Dictionary shinymodule UI
#'
#' Shinymodule UI for the Dictionary tab.
#'
#' @param id Namespace argument.
#'
#' @noRd
dictUI <- function(id) {
  ns <- NS(id)
  tabPanel("Dictionary",
           sidebarLayout(
             sidebarPanel(width = 3,
                    h3("Dictionary Entries"),
                    textInput(NS(id,"input_value"), label = "Allowed input value", placeholder = "Value", width = "250px") %>%
                      prompter::add_prompt(position = "right", message = "Add the allowed value in its initial language."),
                    br(),
                    textInput(NS(id,"input_value_eng"), label = "Allowed input value (Eng)", placeholder = "Value in English", width = "250px") %>%
                      prompter::add_prompt(position = "right", message = "Add the allowed value in English."),
                    br(),
                    actionButton(NS(id,"add"), label = "Add Dictionary Entry") %>%
                      prompter::add_prompt(position = "right",
                                           message = "Fill in the fields above and click the button to add a dictionary entry. IDs are added automatically."),
                    br(),br(),
                    deleteUI(NS(id,"delete")),
             ),
             mainPanel(width = 9,
                tableUI(NS(id,"tbl"))
             )
          )
  )
}

#' Dictionary shinymodule server
#'
#' Shinymodule Server for the Dictionary tab.
#'
#' @param id Namespace argument.
#' @param metadata Metadata object, which contains the data as an
#'   `epic_metadata` class object. This argument is reactive.
#'
#' @noRd
dictServer <- function(id, metadata) {
  stopifnot(is.reactive(metadata))
  metadata_part <- "dict"

  # moduleServer
  moduleServer(id, function(input, output, session) {

    tableServer("tbl", metadata, metadata_part,
                editable = list(target = "cell", disable = list(columns = c(0))),
                options = list(scrollY = '750px', scrollCollapse = TRUE,
                               paging = FALSE, searchHighlight = TRUE))

    deleteServer("delete", metadata, metadata_part)

    # observeEvent(input$add, {
    #   metadata_temp <- metadata()
    #
    #   if (input$input_key %>% stringr::str_trim("both") %>% magrittr::equals("")) {
    #     shinyalert::shinyalert(title = "Warning!", text = "The keyword is empty.", type = "warning")
    #   }
    #   if (input$input_key %>% stringr::str_trim("both") %>% stringr::str_to_lower() %>% magrittr::is_in(metadata_temp[[metadata_part]][[paste0("key_",metadata_part)]] %>% stringr::str_trim("both") %>% stringr::str_to_lower())) {
    #     shinyalert::shinyalert(title = "Warning!", text = "This dict segment already exists.", type = "warning")
    #     updateTextInput(session, inputId = "input_key", label = NULL, value = "")
    #     updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
    #   }
    #   if ((input$input_key %>% stringr::str_trim("both") %>% stringr::str_to_lower() %>% magrittr::is_in(metadata_temp[[metadata_part]][[paste0("key_",metadata_part)]] %>% stringr::str_trim("both") %>% stringr::str_to_lower()) %>% magrittr::not()) &
    #       input$input_key %>% stringr::str_trim("both") %>% magrittr::equals("") %>% magrittr::not()) {
    #     to_add <- data.frame(id_dict = fun_get_id(metadata_temp[[metadata_part]][[paste0("id_",metadata_part)]],"dict"),
    #                          key_dict = input$input_key %>% stringr::str_trim("both"),
    #                          desc_dict = input$input_desc %>% stringr::str_trim("both"))
    #
    #     metadata_temp[[metadata_part]] <- rbind(metadata_temp[[metadata_part]],to_add)
    #     metadata(metadata_temp)
    #
    #     updateTextInput(session, inputId = "input_key", label = NULL, value = "")
    #     updateTextInput(session, inputId = "input_desc", label = NULL, value = "")
    #   }
    # })
  })
}

#' DICT shinymodule demo app
#'
#' Demo app for the shinymodule of the DICT tab, used for testing only.
#'
#' @noRd
dictApp <- function() {
  ui <- tagList(
    prompter::use_prompt(),
    navbarPage(
      title = "Create Steps for SCC Models",
      dictUI("dict_tab")
    )
  )

  server <- function(input, output, session) {
    metadata <- reactiveVal(empty_metadata())

    dictServer("dict_tab", metadata)

  }

  shinyApp(ui, server)
}
