#' Construct `epicdata_meta` objects
#'
#' Constructor function for S3 class `epicdata_meta`. The class is used
#' with the Meta Data Creator `shiny` app to store the inputed information.
#'
#' @details `epicdata_meta` objects are lists containing 6 data.frames.
#'   These data.frames are described below:
#'
#'   `main` contains
#'
#'   `missing_codes` contains
#'
#'   `missing_rules` contains
#'
#'   `contras` contains
#'
#'   `mc_sets` contains
#'
#'   `dict` contains
#'
#' @param x A list to be converted to class `epicdata_meta`.
#'
#' @returns A `epicdata_meta` object. Its structure is described in
#'   Details.
#'
#' @seealso [validate_metadata()], [print.epicdata_meta()],
#'   [empty_metadata()]
#'
#' @export
#'
#' @examples
#' x <- list(data.frame(a = c(1,2,3), b = c(4,5,6)),
#' data.frame(a = c("a","b","c"), b = c("d","e","f")))
#' metadata <- new_metadata(x)
new_metadata <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "epicdata_meta")
}

#' Validate `epicdata_meta` objects
#'
#' Validates that the provided input object of class `epicdata_meta` has
#' the correct structure.
#'
#' @param x An `epicdata_meta` or `epicdata_meta_checked` object to be
#'   validated.
#'
#' @returns An `epicdata_meta` or `epicdata_meta_checked` object, that
#'   has been checked to have the correct structure.
#'
#' @seealso [new_metadata()] for information on `epicdata_meta` objects
#'
#' @export
#'
#' @examples
#' x <- empty_metadata()
#' x <- validate_metadata(x)
validate_metadata <- function(x) {

  class_input <- class(x)
  if (class_input %>% magrittr::is_in(c("epicdata_meta_checked","epicdata_meta")) %>% magrittr::not()) {
    cli::cli_abort("{.var x} must be a metadata object!", class = "no_metadata")
  }

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_subset(names(x), c("main","missing_codes","missing_rules","contras","mc_sets","dict"), add = coll)

  checkmate::assert_data_frame(x[["main"]], ncols = 9, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["main"]]), c("id_main","label_main","label_main_eng","data_type_main","na_else_main","value_labels_main","value_labels_main_eng","range_main","ops_main"), add = coll)

  checkmate::assert_data_frame(x[["missing_codes"]], ncols = 3, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["missing_codes"]]), c("id_missing_codes","label_missing_codes","class_missing_codes"), add = coll)

  checkmate::assert_data_frame(x[["missing_rules"]], ncols = 4, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["missing_rules"]]), c("id_missing_rules","var_missing_rules","code_missing_rules","value_missing_rules"), add = coll)

  checkmate::assert_data_frame(x[["contras"]], ncols = 6, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["contras"]]), c("id_contras","var1_contras","var2_contras","rule_contras","label_contras","solution_contras"), add = coll)

  checkmate::assert_data_frame(x[["mc_sets"]], ncols = 2, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["mc_sets"]]), c("id_mc_sets","vars_mc_sets"), add = coll)

  checkmate::assert_data_frame(x[["dict"]], ncols = 4, col.names = "named", add = coll)
  checkmate::assert_subset(colnames(x[["dict"]]), c("id_dict","var_dict","value_dict","value_dict_eng"), add = coll)

  checkmate::reportAssertions(coll)

  return(x)
}

#' Create empty `epicdata_meta` object
#'
#' Creates an empty `epicdata_meta` object with the correct structure. It
#' is used when starting up the Meta Data Creator `shiny` app.
#'
#' @returns A empty `epicdata_meta` object with correct structure.
#'
#' @seealso [new_metadata()] for information on `epicdata_meta` objects
#'
#' @export
#'
#' @examples
#' x <- empty_metadata()
empty_metadata <- function() {
  x <- vector(mode = "list", length = 6)

  # main
  main_vars <- c("id_main","label_main","label_main_eng","data_type_main","na_else_main","value_labels_main","value_labels_main_eng","range_main","ops_main")
  x[[1]] <- matrix(c(rep("NA", 3), "string", rep("NA",length(main_vars) - 4)), nrow = 1, ncol = length(main_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(main_vars)
  names(x)[1] <- "main"

  # missing_codes
  missing_codes_vars <- c("id_missing_codes","label_missing_codes","class_missing_codes")
  x[[2]] <- matrix(rep("NA", length(missing_codes_vars)), nrow = 1, ncol = length(missing_codes_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(missing_codes_vars)
  names(x)[2] <- "missing_codes"

  # missing_rules
  missing_rules_vars <- c("id_missing_rules","var_missing_rules","code_missing_rules","value_missing_rules")
  x[[3]] <- matrix(c("r0",rep("NA",length(missing_rules_vars) - 1)), nrow = 1, ncol = length(missing_rules_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(missing_rules_vars)
  names(x)[3] <- "missing_rules"

  # contras
  contras_vars <- c("id_contras","var1_contras","var2_contras","rule_contras","label_contras","solution_contras")
  x[[4]] <- matrix(rep("c0",length(contras_vars)), nrow = 1, ncol = length(contras_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(contras_vars)
  names(x)[4] <- "contras"

  # mc_sets
  mc_sets_vars <- c("id_mc_sets","vars_mc_sets")
  x[[5]] <- matrix(c("m0",rep("NA",length(mc_sets_vars) - 1)), nrow = 1, ncol = length(mc_sets_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(mc_sets_vars)
  names(x)[5] <- "mc_sets"

  # dict
  dict_vars <- c("id_dict","var_dict","value_dict","value_dict_eng")
  x[[6]] <- matrix(c("d0",rep("NA",length(dict_vars) - 1)), nrow = 1, ncol = length(dict_vars)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(dict_vars)
  names(x)[6] <- "dict"

  x %<>% new_metadata()
  x %<>% validate_metadata()

  return(x)
}

#' Print `epicdata_meta` objects
#'
#' Print method for objects of class `epicdata_meta`.
#'
#' @param x A `epicdata_meta` object.
#' @inheritParams base::print.default
#'
#' @returns Prints the `epicdata_meta` object in the console.
#'
#' @seealso [new_metadata()] for information on `epicdata_meta` objects
#'
#' @export
#'
#' @examples
#' x <- empty_metadata()
#' print(x)
print.epicdata_meta <- function(x, ...) {
  cli::cli_alert_danger("unchecked (please run {.code check_metadata()} before continuing)")
  cat("The Metadata describes ",nrow(x$main)," variables.\n\n")
  cat("Missing/Jump Codes: ",nrow(x$missing_codes),"\n")
  cat("Missing/Jump Rules: ",nrow(x$missing_rules),"\n")
  cat("Contradictions: ",nrow(x$contras),"\n")
  cat("Multiple Choice Sets: ",nrow(x$mc_sets),"\n")
  cat("Dictionary Entries for String Variables: ",nrow(x$dict),"\n")
}

#' Print `epicdata_meta_checked` objects
#'
#' Print method for objects of class `epicdata_meta_checked`.
#'
#' @param x A `epicdata_meta_checked` object.
#' @inheritParams base::print.default
#'
#' @returns Prints the `epicdata_meta_checked` object in the console.
#'
#' @seealso [new_metadata()] for information on `epicdata_meta` objects
#'
#' @export
#'
#' @examples
#' x <- empty_metadata()
#' x <- check_metadata(x)
#' print(x)
print.epicdata_meta_checked <- function(x, ...) {
  cli::cli_alert_success("checked successfully")
  cat("The Metadata describes ",nrow(x$main)," variables.\n\n")
  cat("Missing/Jump Codes: ",nrow(x$missing_codes),"\n")
  cat("Missing/Jump Rules: ",nrow(x$missing_rules),"\n")
  cat("Contradictions: ",nrow(x$contras),"\n")
  cat("Multiple Choice Sets: ",nrow(x$mc_sets),"\n")
  cat("Dictionary Entries for String Variables: ",nrow(x$dict),"\n")
}

#' Unchecking `epicdata_meta` objects
#'
#' Putting a checked `epicdata_meta` back to an unchecked status.
#'
#' @param metadata An `epicdata_meta` or `epicdata_meta_checked` object.
#'
#' @noRd
uncheck_metadata <- function(metadata) {

  rlang::try_fetch(validate_metadata(metadata),
                   error = function(cnd) {
                     cli::cli_abort("{.var metadata} must be an object of class {.emph epicdata_meta}!",
                                    parent = cnd, class = "no_epicdata_meta")
                   })

  if (inherits(metadata, "epicdata_meta_checked")) {
    metadata %<>% unclass(.)
    metadata %<>% structure(., class = "epicdata_meta")
  }

  return(metadata)
}
