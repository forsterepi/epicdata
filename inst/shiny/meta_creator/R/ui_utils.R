#' UI start tab
#'
#' Defines the UI elements of tab 'Start' in the Metadata Creator `shiny` app.
#'
#' @noRd
ui_start_tab <- function(){
  tabPanel("START",
           h2("Welcome to the Metadata Creator Shiny App in the epicdata package!"),
           br(),
           column(6,
              p("This app helps you to create metadata, which is used in further processing of your data.
               You can just start with an empty list or you can load existing metadata that you previously downloaded."),
              p("If you need help in using the app, you can look at the HELP pages.
               There are also many tooltips available in the app itself."),
              p("When you are done, do not forget to download your metadata! Otherwise your progress will be lost.")
           ),
           column(6,
             wellPanel(
              h4("Upload Metadata"),
              fileInput("upload", label = NULL, buttonLabel = "Upload Metadata",
                        accept = ".rds", placeholder = "Select metadata object (.rds)"),
              h4("Download Metadata"),
              downloadButton("download", label = "Download Metadata"),
              style = "padding: 10px; background: #69d3bf"
            )
        )
  )
}

#' UI main tab
#'
#' Defines the UI elements of tab 'Main' in the Metadata Creator `shiny` app.
#'
#' @noRd
ui_main_tab <- function(){
  tabPanel("Main",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h3("New variable"),
                          textInput("input_id_main", label = "Variable ID", placeholder = "ID", width = "250px") %>%
                            prompter::add_prompt(position = "right", message = "Must be unique. Restrict to lower case letters, digits, and _, if possible."),
                          br(),
                          textAreaInput("input_label_main", label = "Label", placeholder = "Label", height = "200%", resize = "vertical", width = "250px") %>%
                            prompter::add_prompt(position = "right", message = "If questionnaire: question wording; else: deteiled description."),
                          br(),
                          textAreaInput("input_label_main_eng", label = "Label (English)", placeholder = "Label (English)", height = "200%", resize = "vertical", width = "250px") %>%
                            prompter::add_prompt(position = "right", message = "English translation of label."),
                          br(),
                          selectInput("input_data_type_main", label = "Data type", choices = c("string","integer","float","datetime"), width = "250px"),
                          br(),
                          uiOutput("input_main_by_data_type"),
                          actionButton("main_add", "Add"),
                          actionButton("main_delete", "Delete"),
                          actionButton("main_clear", "Clear")
             ),
             mainPanel(width = 9,
                       DT::DTOutput("main_tbl")
             )
           )
  )
}

# ui_step_tab <- function(){
#   empty <- empty_metadata()
#   initial_then_data <- empty$then
#
#   tabPanel("STEP",
#            fluidRow(
#              column(7,
#                     fluidRow(
#                       column(6,
#                              add_if_ifnot_header("if"),
#                              add_if_ifnot_buttons("more_lines_if","less_lines_if"),
#                              wellPanel(
#                                div(id = "if_placeholder",
#                                    div(
#                                      id = "if_row1",
#                                      class = "input-row",
#                                      fluidRow(
#                                        column(2, numericInput("if_numeric_input1", label = NULL, value = 1, min = 1)),
#                                        column(10, selectInput("if_select_input1", label = NULL,
#                                                               choices = c(Choose = "", initial_then_data$desc_then %>% magrittr::extract(order(.)))))
#                                      )
#                                    )),
#                                style = "padding: 10px; background: #69d3bf"
#                              ),
#                              add_if_ifnot_preview("step_if_id", "step_if_desc", "IF")
#                       ),
#                       column(6,
#                              add_if_ifnot_header("ifnot"),
#                              add_if_ifnot_buttons("more_lines_ifnot","less_lines_ifnot"),
#                              wellPanel(
#                                div(id = "ifnot_placeholder",
#                                    div(
#                                      id = "ifnot_row1",
#                                      class = "input-row",
#                                      fluidRow(
#                                        column(2, numericInput("ifnot_numeric_input1", label = NULL, value = 1, min = 1)),
#                                        column(10, selectInput("ifnot_select_input1", label = NULL,
#                                                               choices = c(Choose = "", initial_then_data$desc_then %>% magrittr::extract(order(.)))))
#                                      )
#                                    )),
#                                style = "padding: 10px; background: #69d3bf"
#                              ),
#                              add_if_ifnot_preview("step_ifnot_id", "step_ifnot_desc", "IFNOT")
#                       )
#                     ),
#                     DT::DTOutput("step_tbl")
#              ),
#              column(2,
#                     h4("Create THEN Statement"),
#                     wellPanel(
#                       uiOutput("select_subject"),
#                       uiOutput("select_does"),
#                       uiOutput("select_object"),
#                       uiOutput("select_where"),
#                       style = "padding: 10px; background: #69d3bf"),
#                     h4("Additional STEP Information"),
#                     wellPanel(
#                       checkboxInput("step_input_end", label = "End step?", value = F),
#                       uiOutput("step_input_module"),
#                       textAreaInput("step_input_ref", label = "References", placeholder = "Author et al. 2024",
#                                     height = "200%", resize = "vertical"),
#                       textAreaInput("step_input_note", label = "Notes", placeholder = "Important information and conflicting findings",
#                                     height = "200%", resize = "vertical"),
#                       style = "padding: 10px; background: #69d3bf"
#                     ),
#                     wellPanel(
#                       h5("Actions: STEP"),
#                       actionButton("step_add", label = "Add") %>%
#                         prompter::add_prompt(position = "left", message = "Click to add a STEP. Also adds the THEN statement to the THEN table if it's not
#                                  already available."),
#                       actionButton("step_delete", label = "Delete") %>%
#                         prompter::add_prompt(position = "left", message = "Delete a row from the STEP table by specifying the corresponding ID in the pop-up window."),
#                       actionButton("step_clear", label = "Clear") %>%
#                         prompter::add_prompt(position = "left", message = "Click to revert all inputs to their empty default states."),
#                       style = "padding: 0px; background: #ffffff"
#                     )
#              ),
#              column(3,
#                     add_then_preview("step_then_id","step_then_desc"),
#                     wellPanel(
#                       h5("Actions: THEN"),
#                       actionButton("then_add", label = "Add") %>%
#                         prompter::add_prompt(position = "left", message = "Select Subject, DOES, Object, and WHERE (or some of them) and click the button
#                                   for the THEN statment to be selectable for IF and IFNOT conditions."),
#                       actionButton("then_delete", label = "Delete") %>%
#                         prompter::add_prompt(position = "left", message = "Delete a row from the THEN table by specifying the corresponding ID in the pop-up window."),
#                       style = "padding: 0px; background: #ffffff"
#                     ),
#                     DT::DTOutput("then_tbl"))
#            )
#   )
# }

#' UI help tab
#'
#' Defines the UI elements of tab 'HELP' in the Metadata Creator `shiny` app.
#'
#' @noRd
ui_help_tab <- function() {
  navbarMenu("HELP",
             tabPanel("About Metadata",includeMarkdown("helpfiles/help_about_metadata.Rmd")),
             tabPanel("About Data Types",includeMarkdown("helpfiles/help_about_data_types.Rmd"))
  )
}

# add_text_input <- function(id, type) {
#
#   long <- switch(type,
#                  "key" = "keyword",
#                  "desc" = "description",
#                  "desc_module" = "description",
#                  "Add long version for this type!"
#   )
#   message <- switch(type,
#                     "key" = "Keywords are short descriptions.",
#                     "desc" = "Descriptions are longer than keywords and contain the text that will appear in the STEP descriptions.",
#                     "desc_module" = "Descriptions are longer than keywords.",
#                     "Add message for this type!"
#   )
#   placeholder <- switch(type,
#                         "key" = "Short keyword",
#                         "desc" = "Complete description",
#                         "desc_module" = "Complete description",
#                         "Add placeholder for this type!"
#   )
#
#   wellPanel(h4(paste0("New ",long)) %>%
#               prompter::add_prompt(position = "right", message = message),
#             textInput(id, label = NULL, placeholder = placeholder),
#             style = "padding: 10px; background: #69d3bf")
# }

# add_text_does <- function(id1,id2,id3) {
#   wellPanel(h4("New DOES variations") %>%
#               prompter::add_prompt(position = "right", message = "In order to have a grammatically correct STEP description, add
#                                             the corresponding variations: for WHAT segments in singular, for WHAT segments in plural, and if no
#                                             subject (i.e., the WHAT segment before the DOES segment) is provided."),
#             textInput(id1, label = NULL, placeholder = "For subjects in singular"),
#             textInput(id2, label = NULL, placeholder = "For subjects in plural"),
#             textInput(id3, label = NULL, placeholder = "For missing subjects"),
#             style = "padding: 10px; background: #69d3bf")
# }

# add_check_does <- function(id) {
#   wellPanel(h4("Does this DOES segment need a THEN object?") %>%
#               prompter::add_prompt(position = "right", message = "Some DOES segments, e.g., inhibition, need THEN instead of WHAT objects."),
#             checkboxInput(id, label = "THEN object?", value = F),
#             style = "padding: 10px; background: #69d3bf")
# }

# add_if_ifnot_buttons <- function(id1,id2) {
#   wellPanel(
#     actionButton(id1, "Add statement") %>%
#       prompter::add_prompt(position = "right", message = "Adds new line."),
#     actionButton(id2, "Remove statement") %>%
#       prompter::add_prompt(position = "right", message = "Deletes last line. If only one line is left, clicking the button clears the selection."),
#     style = "padding: 0px; background: #ffffff"
#   )
# }

# add_if_ifnot_header <- function(type = c("if","ifnot")) {
#   type <- match.arg(type)
#
#   if (type == "if") {
#     out <- tagList(
#       h4("Create IF Condition") %>%
#         prompter::add_prompt(position = "right", message = "The IF condition is a collection of available, previously created THEN statments,
#                                        which together must be fulfilled, in order for the STEP to occur.")
#     )
#   }
#
#   if (type == "ifnot") {
#     out <- tagList(
#       h4("Create IFNOT Condition") %>%
#         prompter::add_prompt(position = "right", message = "The IFNOT condition is a collection of available, previously created THEN statments,
#                                        which together must not be fulfilled, in order for the STEP to occur.")
#     )
#   }
#
#   return(out)
# }
