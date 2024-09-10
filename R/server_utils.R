float_range <- function(dropdown, check1, input1, check2, input2) {
  check_input1_greater_input2 <- F

  if (is.null(dropdown)) {
    return(list(out = NA, check_input1_greater_input2 = check_input1_greater_input2))
  }

  if (dropdown == "No Range") {
    out <- "(-Inf;Inf)"
  }

  if (dropdown == "-Inf to ...") {
    if (check2 == TRUE) {
      bracket2 <- "]"
    } else {
      bracket2 <- ")"
    }
    out <- paste0("(-Inf;",input2,bracket2)
  }

  if (dropdown == "... to Inf") {
    if (check1 == TRUE) {
      bracket1 <- "["
    } else {
      bracket1 <- "("
    }
    out <- paste0(bracket1,input1,";Inf)")
  }

  if (dropdown == "... to ...") {
    if (input1 > input2) {
      check_input1_greater_input2 <- T
    }

    if (check1 == TRUE) {
      bracket1 <- "["
    } else {
      bracket1 <- "("
    }
    if (check2 == TRUE) {
      bracket2 <- "]"
    } else {
      bracket2 <- ")"
    }
    out <- paste0(bracket1,input1,";",input2,bracket2)
  }

  return(list(out = out, check_input1_greater_input2 = check_input1_greater_input2))
}

datetime_range <- function(dropdown, check1, input1, check2, input2) {
  check_input1_greater_input2 <- F

  if (is.null(dropdown)) {
    return(list(out = NA, check_input1_greater_input2 = check_input1_greater_input2))
  }

  if (dropdown == "No Range") {
    out <- "(-Inf;Inf)"
  }

  if (dropdown == "-Inf to ...") {
    if (check2 == TRUE) {
      bracket2 <- "]"
    } else {
      bracket2 <- ")"
    }
    out <- paste0("(-Inf;",format(input2,"%d.%m.%Y"),bracket2)
  }

  if (dropdown == "... to Inf") {
    if (check1 == TRUE) {
      bracket1 <- "["
    } else {
      bracket1 <- "("
    }
    out <- paste0(bracket1,format(input1,"%d.%m.%Y"),";Inf)")
  }

  if (dropdown == "... to ...") {
    if (input1 > input2) {
      check_input1_greater_input2 <- T
    }

    if (check1 == TRUE) {
      bracket1 <- "["
    } else {
      bracket1 <- "("
    }
    if (check2 == TRUE) {
      bracket2 <- "]"
    } else {
      bracket2 <- ")"
    }
    out <- paste0(bracket1,format(input1,"%d.%m.%Y"),";",format(input2,"%d.%m.%Y"),bracket2)
  }

  return(list(out = out, check_input1_greater_input2 = check_input1_greater_input2))
}

cat_list <- function(num_input, text_input) {

  check_dupli_num <- F
  check_dupli_text <- F
  check_integer_but_empty <- F

  if (is.null(text_input[[1]])) {
    return(list(out = NA, check_dupli_num = check_dupli_num,
                check_dupli_text = check_dupli_text, check_integer_but_empty = check_integer_but_empty))
  }

  num_input %<>% unlist() %>% as.character()
  text_input %<>% unlist() %>% stringr::str_trim("both")

  input <- cbind(num_input,text_input) %>% as.data.frame()
  input %<>% dplyr::filter(.data$text_input != "")

  if (nrow(input) == 0) {
    check_integer_but_empty <- T
    return(list(out = NA, check_dupli_num = check_dupli_num,
                check_dupli_text = check_dupli_text, check_integer_but_empty = check_integer_but_empty))
  }

  if (input[,1] %>% duplicated() %>% all_false() %>% magrittr::not()) {
    check_dupli_num <- T
  }
  if (input[,2] %>% duplicated() %>% all_false() %>% magrittr::not()) {
    check_dupli_text <- T
  }

  combi <- paste0(input[,1]," = ",input[,2])
  out <- combi %>% stringr::str_c(collapse = " | ")

  return(list(out = out, check_dupli_num = check_dupli_num,
              check_dupli_text = check_dupli_text, check_integer_but_empty = check_integer_but_empty))
}

#' Get list of variables of certain type
#'
#' Processes the metadata to present the list of variables with a certain data
#' type.
#'
#' @param metadata The current metadata, i.e., metadata().
#'
#' @noRd
get_vars_of_type <- function(metadata, type) {

  checkmate::assert_character(type, min.chars = 1, len = 1, any.missing = F, null.ok = F)

  temp <- metadata[["main"]] %>%
    dplyr::filter(.data$data_type_main == type) %>%
    dplyr::select(dplyr::all_of("id_main"))

  if (nrow(temp) > 0) {
    out <- temp$id_main %>% magrittr::extract(temp$id_main %>% stringr::str_to_lower() %>% order())
  } else {
    out <- ""
  }
  return(out)
}

#' Create automatic IDs
#'
#' Automatically creates the next ID for datasets missing_rules, contras,
#' mc_sets, dict in the Metadata Creator `shiny` app.
#'
#' @param id_col A character vector. The existing list of IDs from the
#'   missing_rules, contras, mc_sets, dict table, from which the next ID is
#'   concluded.
#' @param metadata_part A single element from the following options:
#'   'missing_rules','contras','mc_sets','dict'. If the corresponding data.frame
#'   behind id_col is empty, the first ID is created for the specified
#'   metadata_part, e.g., 'c1' for metadata_part 'contras'.
#'
#' @returns A single element of type character containing the next ID in the
#'   input column.
#'
#' @noRd
fun_get_id <- function(id_col, metadata_part = c("missing_rules","contras","mc_sets","dict")){

  metadata_part <- match.arg(metadata_part)

  # Handle "", NA, NULL, other non-character inputs
  if (is.null(id_col)) {
    id_col <- character(0)
  }
  if (!is.character(id_col)) {
    id_col <- character(0)
  }
  if (length(id_col) == 1) {
    if (id_col == "") {
      id_col <- character(0)
    }}

  # Get letter, i.e., first character of ID (WHAT -> a, DOES -> d, WHERE -> e, Module -> m, ICC -> i)
  if (length(id_col) == 0) {
    letter_to_use <- switch(metadata_part,
                            "missing_rules" = "r",
                            "contras" = "c",
                            "mc_sets" = "m",
                            "dict" = "d"
    )
  } else {
    letter_to_use <- id_col %>% stringr::str_sub(1,1) %>% unique()
  }

  # Get newest number
  if (length(id_col) == 0) {
    number_to_use <- "1"
  } else {
    number_to_use <- id_col %>% stringr::str_sub(2,-1) %>% as.numeric() %>% max(na.rm = T) %>% magrittr::add(1)
  }

  ## Create ID
  out <- paste0(letter_to_use,number_to_use)

  # Return
  return(out)
}
