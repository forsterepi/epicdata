#' Static analysis of YAML metadata specification
#'
#' @param file
#'
#' @returns
#' @export
#'
#' @examples
check.yaml <- function(file = NULL) {
  # Read file or active file
  if (is.null(file)) {
    rlang::check_installed(c("rstudioapi"))
    activefile <- rstudioapi::getSourceEditorContext()
    if (activefile$path %>%
      stringi::stri_trans_tolower() %>%
      stringi::stri_detect_regex("\\.yml$|\\.yaml$")) {
      lines <- activefile[["contents"]]
    } else {
      cli::cli_abort(
        "Please provide a .yml or .yaml file!",
        call = rlang::caller_env(),
        class = "error.check.yaml.1"
      )
    }
  } else {
    rlang::try_fetch(
      {
        checkmate::assert_file_exists(
          file,
          access = "r",
          extension = c("yml", "yaml")
        )
      },
      error = function(cnd) {
        cli::cli_abort(
          cnd$message,
          call = rlang::caller_env(),
          class = "error.check.yaml.2"
        )
      }
    )
    lines <- readLines(con = file)
  }

  # Remove comments
  lines %<>% remove.comments.yaml.lines()

  # Trim the rigth side
  lines %<>% stringi::stri_trim_right()

  # Start checks ----

  out <- list()

  # Start validating the first layer, i.e., the components
  out[[length(out) + 1]] <- cy.hierarchie(lines)


  out[[length(out) + 1]] <- cy.comments(lines)

  # NOTE: I need to keep empty lines in, because otherwise line numbers are wrong.
  # Therefore, empty lines cannot cause any errors or warnings.


  out <- do.call(rbind, out)

  if (is.null(out)) {
    cli::cli_alert_success("Metadata is specified correctly!")
    return(invisible(NULL))
  } else {
    if (is.null(file)) {
      out %<>%
        dplyr::mutate(
          file = activefile$path,
          column = 1
        ) %>%
        dplyr::select(type, file, line, column, message)
      rstudioapi::sourceMarkers(
        name = "check.yaml()",
        markers = out
      )
    } else {
      out
    }
  }
}

#' Remove comments
#'
#' Desc.
#'
#' @param x Desc.
#'
#' @returns Desc.
#'
#' @noRd
remove.comments.yaml.lines <- function(lines) {
  lines %<>% stringi::stri_replace_all_regex(
    pattern = "\\s+#.*",
    replacement = ""
  )
  lines %<>% stringi::stri_replace_all_regex(
    pattern = "^#.*",
    replacement = ""
  )

  lines
}

#' Title
#'
#' Desc.
#'
#' @param lines Desc.
#'
#' @returns Desc.
#'
#' @noRd
cy.comments <- function(lines, type = "warning") {
  message <- "[Potential invalid comment] Contains a # symbol, but no comment. Remember that comments need space in front of #."

  which_lines <- which(lines %>% stringi::stri_detect_regex("(?<!\\s)#"))

  if (length(which_lines) > 0) {
    out <- data.frame(
      type = rep(type, length(which_lines)),
      line = which_lines,
      message = rep(message, length(which_lines))
    )
  } else {
    out <- NULL
  }

  out
}


## This function is not completed and tested adequately, especially the j <- j - 1 shananigans
cy.hierarchie <- function(lines, type = "error") {
  message <- "[Error in hierarchie] The indentation does not lead to a valid YAML file."

  # Length of indentation, i.e., number of spaces
  ind <- lines %>%
    stringi::stri_extract_first_regex("^\\s*") %>%
    nchar()

  # Empty lines need to be ignored
  ind[lines == ""] <- NA

  # Ranks
  lvl <- ind %>% unique() %>% sort()
  layer <- ind
  for (i in seq_along(layer)) {
    if (!is.na(layer[i])) {
      layer[i] <- which(lvl == layer[i])
    }
  }

  # Remember and remove NAs
  pos_na <- which(is.na(layer))

  layer <- layer[!is.na(layer)]

  check_hierarchie <- rep(NA, length(layer))

  if (layer[1] == 1) {
    check_hierarchie[1] <- FALSE
  } else {
    check_hierarchie[1] <- TRUE
  }

  for (i in 2:length(layer)) {

    if (layer[i] >= layer[i - 1]) {
      check_hierarchie[i] <- FALSE
    } else {
      j <- i - 1

      while (j > 1) {
        last_layer <- layer[j - 1]

        if (last_layer != layer[i]) {
          break
        }

        j <- j - 1
      }

      if (layer[i] <= last_layer) {
        check_hierarchie[i] <- FALSE
      } else {
        check_hierarchie[i] <- TRUE
      }
    }
  }

  # Put NAs back in
  if (length(pos_na) > 0) {
    for (i in 1:(length(pos_na) - 1)) {
      check_hierarchie <- c(check_hierarchie[1:(pos_na[i] - 1)],
                            NA,
                            check_hierarchie[pos_na[i]:length(check_hierarchie)])
    }

    if (length(check_hierarchie) == (pos_na[length(pos_na)] - 1)) {
      check_hierarchie <- c(check_hierarchie, NA)
    } else {
      check_hierarchie <- c(check_hierarchie[1:(pos_na[length(pos_na)] - 1)],
                            NA,
                            check_hierarchie[pos_na[length(pos_na)]:length(check_hierarchie)])
    }
  }

  # Return
  which_lines <- which(check_hierarchie)

  if (length(which_lines) > 0) {
    out <- data.frame(
      type = rep(type, length(which_lines)),
      line = which_lines,
      message = rep(message, length(which_lines))
    )
  } else {
    out <- NULL
  }

  out
}
