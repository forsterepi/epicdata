float_range <- function(dropdown, check1, input1, check2, input2) {
  check_input1_greater_input2 <- F

  if (is.null(dropdown)) {
    return(list(out = NA,check_input1_greater_input2 = check_input1_greater_input2))
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

  return(list(out = out,check_input1_greater_input2 = check_input1_greater_input2))
}

datetime_range <- function(dropdown, check1, input1, check2, input2) {
  check_input1_greater_input2 <- F

  if (is.null(dropdown)) {
    return(list(out = NA,check_input1_greater_input2 = check_input1_greater_input2))
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

  return(list(out = out,check_input1_greater_input2 = check_input1_greater_input2))
}
