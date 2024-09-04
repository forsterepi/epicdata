#' Server of Metadata Creator `shiny` app
#'
#' The UI of Metadata Creator `shiny` app including server modules.
#'
#' @noRd
server <- function(input, output, session) {
  # Initialize metadata
  metadata <- reactiveVal(empty_metadata())

  # Upload
  observeEvent(input$upload, {
    ## Actual upload
    check_if_rds(input$upload)
    upload_content <- readRDS(input$upload$datapath)
    check_if_metadata(upload_content)
    metadata(upload_content)

  #   ## Update first line of IF / IFNOT / OUTC
  #   n <- n_if()
  #   if (n > 1) {
  #     for (i in 2:n) {
  #       removeUI(selector = paste0("#if_row", i))
  #     }
  #   }
  #   n_if(1)
  #   updateSelectInput(session, "if_select_input1", label = NULL,
  #                     c(Choose = "", steplist()[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #
  #   n <- n_ifnot()
  #   if (n > 1) {
  #     for (i in 2:n) {
  #       removeUI(selector = paste0("#ifnot_row", i))
  #     }
  #   }
  #   n_ifnot(1)
  #   updateSelectInput(session, "ifnot_select_input1", label = NULL,
  #                     c(Choose = "", steplist()[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #
  #   n <- n_outc()
  #   if (n > 1) {
  #     for (i in 2:n) {
  #       removeUI(selector = paste0("#outc_row", i))
  #     }
  #   }
  #   n_outc(1)
  #   updateSelectInput(session, "outc_select_input1", label = NULL, selected = "",
  #                     choices = c(Choose = "", steplist() %>% get_options_outc()))
  })

  # Download
  output$download <- downloadHandler(
    filename = function() {
      "metadata.rds"
    },
    content = function(file) {
      metadata_temp <- metadata()
      check_if_metadata(metadata_temp)
      metadata_temp %<>% uncheck_metadata()
      saveRDS(metadata_temp, file)
    }
  )

  # missing codes
  missing_codesServer("missing_codes_tab", metadata)

  # dict
  dictServer("dict_tab", metadata)

  # main

  # main input by data type -------------------------------------------------
  observeEvent(input$input_data_type_main, {
    if (input$input_data_type_main == "string") {
      output$input_main_by_data_type <- renderUI({
        tagList(
          selectInput("input_na_else_main", label = "NA else", width = "250px",
                      choices = c(Choose = "", metadata()[["missing_codes"]]$id_missing_codes %>%
                                    magrittr::extract(metadata()[["missing_codes"]]$id_missing_codes %>% stringr::str_detect("^01\\.01\\.") %>% magrittr::not()) %>%
                                    magrittr::extract(order(.)))),
          checkboxInput("input_ops_main", label = "'Other: please specify' field?", value = F)
        )
      })
    }
    if (input$input_data_type_main == "integer") {
      output$input_main_by_data_type <- renderUI({
        tagList(
          selectInput("input_na_else_main", label = "NA else", width = "250px",
                      choices = c(Choose = "", metadata()[["missing_codes"]]$id_missing_codes %>%
                                    magrittr::extract(metadata()[["missing_codes"]]$id_missing_codes %>% stringr::str_detect("^01\\.01\\.") %>% magrittr::not()) %>%
                                    magrittr::extract(order(.)))),
          fluidRow(
            column(1),
            column(11,
            actionButton("more_lines_cat", "+"),
            actionButton("less_lines_cat", "-")
          )),
          br(),
          div(id = "cat_placeholder",
              div(
                id = "cat_row1",
                class = "input-row",
                tagList(
                  numericInput("input_cat_numeric1", label = NULL, value = 1, min = 0, step = 1),
                  textInput("input_cat_label1", label = NULL, placeholder = "Name of category"),
                  textInput("input_cat_label_eng1", label = NULL, placeholder = "Name of category (English)")
                )
              ))
        )
      })
    }
    if (input$input_data_type_main == "float") {
      output$input_main_by_data_type <- renderUI({
        tagList(
          selectInput("input_na_else_main", label = "NA else", width = "250px",
                      choices = c(Choose = "", metadata()[["missing_codes"]]$id_missing_codes %>%
                                    magrittr::extract(metadata()[["missing_codes"]]$id_missing_codes %>% stringr::str_detect("^01\\.01\\.") %>% magrittr::not()) %>%
                                    magrittr::extract(order(.)))),
          selectInput("input_float_range_select", label = "Range", width = "250px",
                      choices = c("No Range","-Inf to ...", "... to Inf", "... to ...")),
          uiOutput("input_float_range")
        )
      })
    }
    if (input$input_data_type_main == "datetime") {
      output$input_main_by_data_type <- renderUI({
        tagList(
          selectInput("input_na_else_main", label = "NA else", width = "250px",
                      choices = c(Choose = "", metadata()[["missing_codes"]]$id_missing_codes %>%
                                    magrittr::extract(metadata()[["missing_codes"]]$id_missing_codes %>% stringr::str_detect("^01\\.01\\.")) %>%
                                    magrittr::extract(order(.)))),
          selectInput("input_datetime_range_select", label = "Range", width = "250px",
                      choices = c("No Range","-Inf to ...", "... to Inf", "... to ...")),
          uiOutput("input_datetime_range")
        )
      })
    }
  })

  observeEvent(input$input_float_range_select, {
    if (input$input_float_range_select == "No Range") {
      output$input_float_range <- renderUI({
        tagList()
      })
    }
    if (input$input_float_range_select == "-Inf to ...") {
      output$input_float_range <- renderUI({
        tagList(
          fluidRow(
            column(3,checkboxInput("input_float_range2_smaller_equal", label = "<=", value = F)),
            column(9,numericInput("input_float_range2", label = NULL, value = 0, step = 0.1, width = "150px"))
          )
        )
      })
    }
    if (input$input_float_range_select == "... to Inf") {
      output$input_float_range <- renderUI({
        tagList(
          fluidRow(
            checkboxInput("input_float_range1_greater_equal", label = ">=", value = F),
            numericInput("input_float_range1", label = NULL, value = 0, step = 0.1, width = "150px")
          )
        )
      })
    }
    if (input$input_float_range_select == "... to ...") {
      output$input_float_range <- renderUI({
        tagList(
          fluidRow(
            checkboxInput("input_float_range1_greater_equal", label = ">=", value = F),
            numericInput("input_float_range1", label = NULL, value = 0, step = 0.1, width = "150px")
          ),
          fluidRow(
            checkboxInput("input_float_range2_smaller_equal", label = "<=", value = F),
            numericInput("input_float_range2", label = NULL, value = 0, step = 0.1, width = "150px")
          )
        )
      })
    }
  })

  observeEvent(input$input_datetime_range_select, {
    if (input$input_datetime_range_select == "No Range") {
      output$input_datetime_range <- renderUI({
        tagList()
      })
    }
    if (input$input_datetime_range_select == "-Inf to ...") {
      output$input_datetime_range <- renderUI({
        tagList(
          dateInput("input_datetime_range2", label = NULL, format = "dd. M yyyy", weekstart = 1)
        )
      })
    }
    if (input$input_datetime_range_select == "... to Inf") {
      output$input_datetime_range <- renderUI({
        tagList(
          dateInput("input_datetime_range1", label = NULL, format = "dd. M yyyy", weekstart = 1)
        )
      })
    }
    if (input$input_datetime_range_select == "... to ...") {
      output$input_datetime_range <- renderUI({
        tagList(
          dateInput("input_datetime_range1", label = NULL, format = "dd. M yyyy", weekstart = 1),
          dateInput("input_datetime_range2", label = NULL, format = "dd. M yyyy", weekstart = 1)
        )
      })
    }
  })

  # main table --------------------------------------------------------------
  output$main_tbl <- DT::renderDataTable(metadata()[["main"]], server = T, rownames = F, selection = "none", editable = "cell",
                                         options = list(scrollY = '500px', scrollCollapse = TRUE,
                                         paging = FALSE, order = list(0,'asc'),
                                         searchHighlight = TRUE))

  # Cat Input ---------------------------------------------------------------
  n_cat <- reactiveVal(1)

  ## more_lines_cat
  observeEvent(input$more_lines_cat ,{
    n <- n_cat() + 1
    n_cat(n)

    insertUI(
      selector = "#cat_placeholder",
      where = "beforeEnd",
      ui = div(
        id = paste0("cat_row", n),
        class = "input-row",
        tagList(
          numericInput(paste0("input_cat_numeric", n), label = NULL, value = 1, min = 0, step = 1),
          textInput(paste0("input_cat_label", n), label = NULL, placeholder = "Name of category"),
          textInput(paste0("input_cat_label_eng", n), label = NULL, placeholder = "Name of category (English)")
        )))
  })

  ## less_lines_cat
  observeEvent(input$less_lines_cat ,{
    n <- n_cat()

    if (n > 1) {
      removeUI(
        selector = paste0("#cat_row", n)
      )
      n_cat(n - 1)
    } else {
      updateNumericInput(session, "input_cat_numeric1", label = NULL, value = 1, min = 0, step = 1)
      updateTextInput(session,"input_cat_label1", label = NULL, placeholder = "Name of category", value = "")
      updateTextInput(session,"input_cat_label_eng1", label = NULL, placeholder = "Name of category (English)", value = "")
    }
  })

  # # THEN Add ----------------------------------------------------------------
  # observeEvent(input$then_add, {
  #   steplist_temp <- steplist()
  #
  #   id_then_temp <- fun_create_then_step_id(subject_key = input$select_subject, does_key = input$select_does,
  #                                           object_key = (object_key() %||% ""), where_key = input$select_where,
  #                                           check_object = (check_object() %||% ""), what_data = steplist_temp[["what"]],
  #                                           does_data = steplist_temp[["does"]], where_data = steplist_temp[["where"]],
  #                                           then_data = steplist_temp[["then"]])
  #   desc_then_temp <- fun_create_then_step_desc(subject_key = input$select_subject, does_key = input$select_does,
  #                                               object_key = (object_key() %||% ""), where_key = input$select_where,
  #                                               check_object = (check_object() %||% ""), what_data = steplist_temp[["what"]],
  #                                               does_data = steplist_temp[["does"]], where_data = steplist_temp[["where"]],
  #                                               then_data = steplist_temp[["then"]])
  #   if (desc_then_temp == "") {
  #     shinyalert::shinyalert(title = "Warning!", text = "The THEN statement is empty.", type = "warning")
  #   }
  #   if (id_then_temp %>% magrittr::is_in(steplist_temp[["then"]]$id_then)) {
  #     shinyalert::shinyalert(title = "Warning!", text = "This THEN statement already exists.", type = "warning")
  #     updateSelectInput(session, inputId = "select_subject", "Subject (WHAT)", selected = "")
  #     updateSelectInput(session, inputId = "select_does", "DOES", selected = "")
  #     updateSelectInput(session, inputId = "select_where", "WHERE", selected = "")
  #     object_what("")
  #     object_then("")
  #   }
  #   if ((id_then_temp %>% magrittr::is_in(steplist_temp[["then"]]$id_then) %>% magrittr::not()) & desc_then_temp != "") {
  #     to_add <- data.frame(id_then = id_then_temp,
  #                          desc_then = desc_then_temp)
  #
  #     steplist_temp[["then"]] <- rbind(steplist_temp[["then"]],to_add)
  #     steplist(steplist_temp)
  #
  #     ## Clear input fields
  #     updateSelectInput(session, inputId = "select_subject", "Subject (WHAT)", selected = "")
  #     updateSelectInput(session, inputId = "select_does", "DOES", selected = "")
  #     updateSelectInput(session, inputId = "select_where", "WHERE", selected = "")
  #     object_what("")
  #     object_then("")
  #
  #     ## Clear and update if
  #     n <- n_if()
  #     if (n > 1) {
  #       for (i in 2:n) {
  #         removeUI(selector = paste0("#if_row", i))
  #       }
  #     }
  #     n_if(1)
  #     updateSelectInput(session, "if_select_input1", label = NULL, selected = "",
  #                       choices = c(Choose = "", steplist_temp[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #     updateNumericInput(session, "if_numeric_input1", label = NULL, value = 1)
  #
  #     ## Clear and update ifnot
  #     n <- n_ifnot()
  #     if (n > 1) {
  #       for (i in 2:n) {
  #         removeUI(selector = paste0("#ifnot_row", i))
  #       }
  #     }
  #     n_ifnot(1)
  #     updateSelectInput(session, "ifnot_select_input1", label = NULL, selected = "",
  #                       choices = c(Choose = "", steplist_temp[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #     updateNumericInput(session, "ifnot_numeric_input1", label = NULL, value = 1)
  #   }
  # })
  #
  #
  # # THEN Delete -------------------------------------------------------------
  # observeEvent(input$then_delete, {
  #   shinyalert::shinyalert(title = "Delete row", type = "input", inputType = "text", inputPlaceholder = "Provide id_then",
  #                          callbackR = function(then_delete_id) {
  #                            steplist_temp <- steplist()
  #                            steplist_temp[["then"]] %<>% dplyr::filter(id_then != then_delete_id)
  #                            steplist(steplist_temp)
  #
  #                            ## Clear and update if
  #                            n <- n_if()
  #                            if (n > 1) {
  #                              for (i in 2:n) {
  #                                removeUI(selector = paste0("#if_row", i))
  #                              }
  #                            }
  #                            n_if(1)
  #                            updateSelectInput(session, "if_select_input1", label = NULL, selected = "",
  #                                              choices = c(Choose = "", steplist_temp[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #                            updateNumericInput(session, "if_numeric_input1", label = NULL, value = 1)
  #
  #                            ## Clear and update ifnot
  #                            n <- n_ifnot()
  #                            if (n > 1) {
  #                              for (i in 2:n) {
  #                                removeUI(selector = paste0("#ifnot_row", i))
  #                              }
  #                            }
  #                            n_ifnot(1)
  #                            updateSelectInput(session, "ifnot_select_input1", label = NULL, selected = "",
  #                                              choices = c(Choose = "", steplist_temp[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #                            updateNumericInput(session, "ifnot_numeric_input1", label = NULL, value = 1)
  #                          }
  #   )
  # })
  #
  # # STEP Table --------------------------------------------------------------
  # output$step_tbl <- DT::renderDataTable(steplist()[["step"]], server = T, rownames = F, selection = "none",
  #                                        editable = list(target = "cell", disable = list(columns = c(0,1))),
  #                                        options = list(scrollY = '380px', scrollCollapse = TRUE,
  #                                                       paging = FALSE, order = list(0,'asc'),
  #                                                       searchHighlight = TRUE))
  #
  # observeEvent(input$step_tbl_cell_edit, {
  #   steplist_temp <- steplist()
  #   steplist_temp[["step"]] <- DT::editData(steplist_temp[["step"]], input$step_tbl_cell_edit, 'step_tbl', rownames = F)
  #   steplist(steplist_temp)
  #
  #   ## Update outc
  #   n <- n_outc()
  #   if (n > 1) {
  #     for (i in 2:n) {
  #       removeUI(selector = paste0("#outc_row", i))
  #     }
  #   }
  #   n_outc(1)
  #   updateSelectInput(session, "outc_select_input1", label = NULL, selected = "",
  #                     choices = c(Choose = "", steplist_temp %>% get_options_outc()))
  # })
  #
  #
  # # STEP Add ----------------------------------------------------------------
  # observeEvent(input$step_add, {
  #   steplist_temp <- steplist()
  #
  #   ## Extract IFNOT inputs
  #   val_ifnot_select <- vector(mode = "character", length = isolate(n_ifnot()))
  #   for (i in 1:isolate(n_ifnot())) {
  #     val_ifnot_select[i] <- lapply(reactiveValuesToList(input), unclass)[paste0("ifnot_select_input",i)] %||% ""
  #   }
  #   val_ifnot_numeric <- vector(mode = "numeric", length = isolate(n_ifnot()))
  #   for (i in 1:isolate(n_ifnot())) {
  #     val_ifnot_numeric[i] <- lapply(reactiveValuesToList(input), unclass)[paste0("ifnot_numeric_input",i)]
  #   }
  #
  #   val_id_ifnot <- fun_create_step_ifnot_id(input_select = val_ifnot_select, input_numeric = val_ifnot_numeric, then_data = steplist()[["then"]])
  #   val_desc_ifnot <- fun_create_step_ifnot_desc(input_select = val_ifnot_select, input_numeric = val_ifnot_numeric)
  #
  #   ## Make IFNOT checks
  #   if (val_id_ifnot[[2]]) {
  #     shinyalert::shinyalert(title = "Warning!", text = "Some scenario of the IFNOT condition contains duplicate statements.", type = "warning")
  #   }
  #   if (val_id_ifnot[[3]]) {
  #     shinyalert::shinyalert(title = "Warning!", text = "Some scenario of the IFNOT condition are identical.", type = "warning")
  #   }
  #
  #   ## Extract IF values
  #   val_if_select <- vector(mode = "character", length = isolate(n_if()))
  #   for (i in 1:isolate(n_if())) {
  #     val_if_select[i] <- lapply(reactiveValuesToList(input), unclass)[paste0("if_select_input",i)] %||% ""
  #   }
  #   val_if_numeric <- vector(mode = "numeric", length = isolate(n_if()))
  #   for (i in 1:isolate(n_if())) {
  #     val_if_numeric[i] <- lapply(reactiveValuesToList(input), unclass)[paste0("if_numeric_input",i)]
  #   }
  #
  #   val_id_if <- fun_create_step_if_id(input_select = val_if_select, input_numeric = val_if_numeric, then_data = steplist()[["then"]])
  #   val_desc_if <- fun_create_step_if_desc(input_select = val_if_select, input_numeric = val_if_numeric)
  #
  #   ## Make IF checks
  #   if (val_id_if[[2]]) {
  #     shinyalert::shinyalert(title = "Warning!", text = "The IF condition contains duplicate statement.", type = "warning")
  #   }
  #   if (val_id_if[[3]]) {
  #     shinyalert::shinyalert(title = "Warning!", text = "Some scenario of the IF condition are identical.", type = "warning")
  #   }
  #
  #   ## Addition process begins, if no warnings
  #   if ((!val_id_if[[2]]) & (!val_id_if[[3]]) & (!val_id_ifnot[[2]]) & (!val_id_ifnot[[3]])) {
  #
  #     ### Get THEN ID and desc
  #     id_then_temp <- fun_create_then_step_id(subject_key = input$select_subject, does_key = input$select_does,
  #                                             object_key = (object_key() %||% ""), where_key = input$select_where,
  #                                             check_object = (check_object() %||% ""), what_data = steplist()[["what"]],
  #                                             does_data = steplist()[["does"]], where_data = steplist()[["where"]],
  #                                             then_data = steplist()[["then"]])
  #     desc_then_temp <- fun_create_then_step_desc(subject_key = input$select_subject, does_key = input$select_does,
  #                                                 object_key = (object_key() %||% ""), where_key = input$select_where,
  #                                                 check_object = (check_object() %||% ""), what_data = steplist()[["what"]],
  #                                                 does_data = steplist()[["does"]], where_data = steplist()[["where"]],
  #                                                 then_data = steplist()[["then"]])
  #
  #     ### Add THEN if not already there
  #     if ((id_then_temp %>% magrittr::is_in(steplist_temp[["then"]]$id_then) %>% magrittr::not()) & desc_then_temp != "") {
  #       to_add_then <- data.frame(id_then = id_then_temp,
  #                                 desc_then = desc_then_temp)
  #
  #       steplist_temp[["then"]] <- rbind(steplist_temp[["then"]],to_add_then)
  #     }
  #
  #     ### STEP addition
  #
  #     to_add_desc_step <- fun_create_step_desc(input_if = val_desc_if, input_ifnot = val_desc_ifnot, input_then = desc_then_temp,
  #                                              input_end_step = as.character(as.numeric(input$step_input_end)))
  #     to_add_id_step <- fun_create_step_id(input_if = val_id_if[[1]], input_ifnot = val_id_ifnot[[1]], input_then = id_then_temp)
  #
  #     if (desc_then_temp == "") {
  #       shinyalert::shinyalert(title = "Warning!", text = "The THEN statement is empty.", type = "warning")
  #     }
  #     if (to_add_id_step %>% magrittr::is_in(steplist_temp[["step"]]$id_step)) {
  #       shinyalert::shinyalert(title = "Warning!", text = "This STEP already exists.", type = "warning")
  #     }
  #     if (to_add_id_step %>% magrittr::is_in(steplist_temp[["step"]]$id_step) %>% magrittr::not() & desc_then_temp != "") {
  #       to_add_step <- data.frame(id_step = to_add_id_step,
  #                                 desc_step = to_add_desc_step,
  #                                 end_step = as.character(as.numeric(input$step_input_end)),
  #                                 module_step = fun_get_module_id(steplist_temp[["module"]],input$step_input_module),
  #                                 note_step = input$step_input_note %>% stringr::str_trim("both"),
  #                                 ref_step = input$step_input_ref %>% stringr::str_trim("both"))
  #
  #       steplist_temp[["step"]] <- rbind(steplist_temp[["step"]],to_add_step)
  #       steplist(steplist_temp)
  #
  #       updateSelectInput(session, inputId = "select_subject", "Subject (WHAT)", selected = "")
  #       updateSelectInput(session, inputId = "select_does", "DOES", selected = "")
  #       updateSelectInput(session, inputId = "select_where", "WHERE", selected = "")
  #       updateTextInput(session, inputId = "step_input_note", label = "Notes", value = "")
  #       updateTextInput(session, inputId = "step_input_ref", label = "References", value = "")
  #       object_what("")
  #       object_then("")
  #
  #       ## Clear if
  #       n <- n_if()
  #       if (n > 1) {
  #         for (i in 2:n) {
  #           removeUI(selector = paste0("#if_row", i))
  #         }
  #       }
  #       n_if(1)
  #       updateSelectInput(session, "if_select_input1", label = NULL, selected = "",
  #                         choices = c(Choose = "", steplist_temp[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #       updateNumericInput(session, "if_numeric_input1", label = NULL, value = 1)
  #
  #       ## Clear ifnot
  #       n <- n_ifnot()
  #       if (n > 1) {
  #         for (i in 2:n) {
  #           removeUI(selector = paste0("#ifnot_row", i))
  #         }
  #       }
  #       n_ifnot(1)
  #       updateSelectInput(session, "ifnot_select_input1", label = NULL, selected = "",
  #                         choices = c(Choose = "", steplist_temp[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #       updateNumericInput(session, "ifnot_numeric_input1", label = NULL, value = 1)
  #
  #       ## Update outc
  #       n <- n_outc()
  #       if (n > 1) {
  #         for (i in 2:n) {
  #           removeUI(selector = paste0("#outc_row", i))
  #         }
  #       }
  #       n_outc(1)
  #       updateSelectInput(session, "outc_select_input1", label = NULL, selected = "",
  #                         choices = c(Choose = "", steplist_temp %>% get_options_outc()))
  #     }
  #   }
  # })
  #
  #
  # # STEP Delete -------------------------------------------------------------
  # observeEvent(input$step_delete, {
  #   shinyalert::shinyalert(title = "Delete row", type = "input", inputType = "text", inputPlaceholder = "Provide id_step",
  #                          callbackR = function(step_delete_id) {
  #                            steplist_temp <- steplist()
  #                            steplist_temp[["step"]] %<>% dplyr::filter(id_step != step_delete_id)
  #                            steplist(steplist_temp)
  #
  #                            ## Update outc
  #                            n <- n_outc()
  #                            if (n > 1) {
  #                              for (i in 2:n) {
  #                                removeUI(selector = paste0("#outc_row", i))
  #                              }
  #                            }
  #                            n_outc(1)
  #                            updateSelectInput(session, "outc_select_input1", label = NULL, selected = "",
  #                                              choices = c(Choose = "", steplist_temp %>% get_options_outc()))
  #                          }
  #   )
  # })
  #
  # # STEP Clear --------------------------------------------------------------
  # observeEvent(input$step_clear, {
  #   updateSelectInput(session, inputId = "select_subject", "Subject (WHAT)", selected = "")
  #   updateSelectInput(session, inputId = "select_does", "DOES", selected = "")
  #   updateSelectInput(session, inputId = "select_where", "WHERE", selected = "")
  #   updateTextInput(session, inputId = "step_input_note", label = "Notes", value = "")
  #   updateTextInput(session, inputId = "step_input_ref", label = "References", value = "")
  #   updateSelectInput(session, inputId = "step_input_module", label = "Module", selected = "")
  #   updateCheckboxInput(session ,inputId = "step_input_end", label = "End step?", value = F)
  #   object_what("")
  #   object_then("")
  #
  #   ## Clear if
  #   n <- n_if()
  #   if (n > 1) {
  #     for (i in 2:n) {
  #       removeUI(selector = paste0("#if_row", i))
  #     }
  #   }
  #   n_if(1)
  #   updateSelectInput(session, "if_select_input1", label = NULL, selected = "",
  #                     choices = c(Choose = "", steplist()[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #   updateNumericInput(session, "if_numeric_input1", label = NULL, value = 1)
  #
  #   ## Clear ifnot
  #   n <- n_ifnot()
  #   if (n > 1) {
  #     for (i in 2:n) {
  #       removeUI(selector = paste0("#ifnot_row", i))
  #     }
  #   }
  #   n_ifnot(1)
  #   updateSelectInput(session, "ifnot_select_input1", label = NULL, selected = "",
  #                     choices = c(Choose = "", steplist()[["then"]]$desc_then %>% magrittr::extract(order(.))))
  #   updateNumericInput(session, "ifnot_numeric_input1", label = NULL, value = 1)
  # })
  #
  #
  # # OUTCOME Input -----------------------------------------------------------
  # n_outc <- reactiveVal(1)
  #
  # ## more_lines_outc
  # observeEvent(input$more_lines_outc ,{
  #   n <- n_outc() + 1
  #   n_outc(n)
  #
  #   insertUI(
  #     selector = "#outc_placeholder",
  #     where = "beforeEnd",
  #     ui = div(
  #       id = paste0("outc_row", n),
  #       class = "input-row",
  #       selectInput(paste0("outc_select_input", n), label = NULL,
  #                   choices = c(Choose = "", steplist() %>% get_options_outc()))
  #     ))
  # })
  #
  # ## less_lines_outc
  # observeEvent(input$less_lines_outc ,{
  #   n <- n_outc()
  #
  #   if (n > 1) {
  #     removeUI(
  #       selector = paste0("#outc_row", n)
  #     )
  #     n_outc(n - 1)
  #   } else {
  #     updateSelectInput(session, "outc_select_input1", label = NULL, selected = "")
  #   }
  # })
  #
  # # OUTCOME Table -----------------------------------------------------------
  # output$outc_tbl <- DT::renderDataTable(steplist()[["outc"]], server = T, rownames = F, selection = "none",
  #                                        options = list(scrollY = '750px', scrollCollapse = TRUE,
  #                                                       paging = FALSE, order = list(0,'asc'),
  #                                                       searchHighlight = TRUE))
  #
  # # OUTCOME Add -------------------------------------------------------------
  # observeEvent(input$outc_add, {
  #   steplist_temp <- steplist()
  #
  #   ## Extract inputs
  #   val_outc_select <- vector(mode = "character", length = isolate(n_outc()))
  #   for (i in 1:isolate(n_outc())) {
  #     val_outc_select[i] <- lapply(reactiveValuesToList(input), unclass)[paste0("outc_select_input",i)] %||% ""
  #   }
  #
  #   to_add_desc_outc <- val_outc_select %>% create_outc_desc(coll = F)
  #
  #   if (to_add_desc_outc %>% create_outc_desc(coll = T) == "") {
  #     shinyalert::shinyalert(title = "Warning!", text = "All inputs are empty.", type = "warning")
  #   } else {
  #
  #     val_outc_id <- vector(mode = "character", length = length(to_add_desc_outc))
  #     for (i in 1:length(val_outc_id)) {
  #       val_outc_id[i] <- to_add_desc_outc[i] %>% get_id_outc(steplist_temp[["step"]])
  #     }
  #
  #     if (val_outc_id %>% unique() %>% length() %>% magrittr::equals(length(val_outc_id)) %>% magrittr::not()) {
  #       shinyalert::shinyalert(title = "Warning!", text = "The outcome definition contains duplicate statements.", type = "warning")
  #     } else {
  #       to_add_id_outc <- val_outc_id %>% stringr::str_c(collapse = "+")
  #
  #       if (check_outc_duplicates(to_add_id_outc, steplist_temp[["outc"]])) {
  #         shinyalert::shinyalert(title = "Warning!", text = "The outcome definition already exists.", type = "warning")
  #       } else {
  #         to_add_outc <- data.frame(id_outc = to_add_id_outc,
  #                                   desc_outc = to_add_desc_outc %>% create_outc_desc(coll = T))
  #
  #         steplist_temp[["outc"]] <- rbind(steplist_temp[["outc"]],to_add_outc)
  #         steplist(steplist_temp)
  #
  #         n <- n_outc()
  #         if (n > 1) {
  #           for (i in 2:n) {
  #             removeUI(selector = paste0("#outc_row", i))
  #           }
  #         }
  #         n_outc(1)
  #         updateSelectInput(session, "outc_select_input1", label = NULL, selected = "",
  #                           choices = c(Choose = "", steplist() %>% get_options_outc()))
  #       }
  #     }
  #   }
  # })
  #
  # # OUTCOME Delete ----------------------------------------------------------
  # observeEvent(input$outc_delete, {
  #   shinyalert::shinyalert(title = "Delete row", type = "input", inputType = "text", inputPlaceholder = "Provide id_outc",
  #                          callbackR = function(outc_delete_id) {
  #                            steplist_temp <- steplist()
  #                            steplist_temp[["outc"]] %<>% dplyr::filter(id_outc != outc_delete_id)
  #                            steplist(steplist_temp)
  #                          }
  #   )
  # })
  #
  # # OUTCOME Clear -----------------------------------------------------------
  # observeEvent(input$outc_clear, {
  #   n <- n_outc()
  #   if (n > 1) {
  #     for (i in 2:n) {
  #       removeUI(selector = paste0("#outc_row", i))
  #     }
  #   }
  #   n_outc(1)
  #   updateSelectInput(session, "outc_select_input1", label = NULL, selected = "",
  #                     choices = c(Choose = "", steplist() %>% get_options_outc()))
  # })

}
