#' Notice variables that are in the data but not in the metadata
#'
#' Variant ID: VAR_DF.NOT.META_NOTE
#'
#' @param dataset A S7 `epicdata::dataset` object
#'
#' @returns A S7 `epicdata::dataset` object
#' @export
#'
#' @examples
VAR_DF.NOT.META_NOTE <- function(dataset) {
  # Check inputs
  S7::check_is_S7(dataset)
  checkmate::assert_class(dataset, "epicdata::dataset", null.ok = FALSE)

  # State of the data (before)
  state.of.the.data(
    d = dataset@processed, metadata = dataset@metadata, when = "before",
    all.vars.character = TRUE
  )

  # Process
  dataset_new <- dataset

  dataset_new@log[["VAR_DF.NOT.META_NOTE"]] <- dataset_new@processed %>%
    colnames() %>%
    magrittr::extract(
      dataset_new@processed %>%
        colnames() %>%
        magrittr::is_in(dataset_new@metadata@var.names) %>%
        magrittr::not()
    )

  # State of the data (after)
  state.of.the.data(
    d = dataset_new@processed, metadata = dataset_new@metadata, when = "after",
    all.vars.character = TRUE
  )

  # Return data
  dataset_new
}

#' Remove variables that are in the data but not in the metadata
#'
#' Variant ID: VAR_DF.NOT.META_RM
#'
#' @param dataset A S7 `epicdata::dataset` object
#'
#' @returns A S7 `epicdata::dataset` object
#' @export
#'
#' @examples
VAR_DF.NOT.META_RM <- function(dataset) {
  # Check inputs
  S7::check_is_S7(dataset)
  checkmate::assert_class(dataset, "epicdata::dataset", null.ok = FALSE)

  # State of the data (before)
  state.of.the.data(
    d = dataset@processed, metadata = dataset@metadata, when = "before",
    all.vars.character = TRUE
  )

  # Process
  dataset_new <- dataset

  to_remove <- dataset_new@processed %>%
    colnames() %>%
    magrittr::extract(
      dataset_new@processed %>%
        colnames() %>%
        magrittr::is_in(dataset_new@metadata@var.names) %>%
        magrittr::not()
    )

  dataset_new@processed %<>%
    dplyr::select(-dplyr::all_of(to_remove))

  dataset_new@log[["VAR_DF.NOT.META_RM"]] <- to_remove

  # State of the data (after)
  state.of.the.data(
    d = dataset_new@processed, metadata = dataset_new@metadata, when = "after",
    all.vars.character = TRUE
  )

  # Return data
  dataset_new
}
