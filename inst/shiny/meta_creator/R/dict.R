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
                    uiOutput(NS(id,"input_var")),
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

    output$input_var <- renderUI(selectInput(NS(id,"input_var"), "Variable", selectize = T,
                                                c(Choose = "", metadata() %>% get_vars_of_type("string"))))

    tableServer("tbl", metadata, metadata_part,
                editable = list(target = "cell", disable = list(columns = c(0))),
                options = list(scrollY = '750px', scrollCollapse = TRUE,
                               paging = FALSE, searchHighlight = TRUE))

    deleteServer("delete", metadata, metadata_part)

    observeEvent(input$add, {
      metadata_temp <- metadata()

      dict_var_empty_fine <- T
      if (input$input_var == "") {
        dict_var_empty_fine <- F
        shinyalert::shinyalert(title = "Warning!", text = "Please specify a variable from the dropdown list.", type = "warning")
      }
      dict_val_empty_fine <- T
      if (input$input_value == "") {
        dict_val_empty_fine <- F
        shinyalert::shinyalert(title = "Warning!", text = "Please specify a value.", type = "warning")
      }
      dict_id_val_combi_exists_fine <- T
      if (paste0(input$input_var,input$input_value) %>% magrittr::is_in(paste0(metadata_temp[["dict"]]$var_dict,metadata_temp[["dict"]]$value_dict))) {
        dict_id_val_combi_exists_fine <- F
        shinyalert::shinyalert(title = "Warning!", text = "This value is already in the list for this variable.", type = "warning")
      }

      if (dict_var_empty_fine & dict_val_empty_fine & dict_id_val_combi_exists_fine) {

        if (input$input_value_eng == "") {
          eng_value <- "NA"
        } else {
          eng_value <- input$input_value_eng %>% stringr::str_trim("both")
        }

        to_add <- data.frame(id_dict = fun_get_id(metadata_temp[["dict"]][["id_dict"]],"dict"),
                             var_dict = input$input_var %>% stringr::str_trim("both"),
                             value_dict = input$input_value %>% stringr::str_trim("both"),
                             value_dict_eng = eng_value)

        metadata_temp[[metadata_part]] <- rbind(metadata_temp[[metadata_part]],to_add)
        metadata(metadata_temp)

        updateTextInput(session, inputId = "input_value", label = "Allowed input value", value = "")
        updateTextInput(session, inputId = "input_value_eng", label = "Allowed input value (Eng)", value = "")
      }
    })
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
