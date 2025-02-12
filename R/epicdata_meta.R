#' Metadata objects
#'
#' Constructor function for S3 class `epicdata_meta`.
#'
#' @details `epicdata_meta` objects are lists containing 7 data.frames.
#'   These data.frames are described below:
#'
#'   `options` contains
#'
#'   `var.list` contains
#'
#'   `na.codes` contains
#'
#'   `na.rules` contains
#'
#'   `contras` contains
#'
#'   `mc.sets` contains
#'
#'   `dict` contains
#'
#' @param x A list to be converted to class `epicdata_meta`.
#' @param ... Additional arguments for generics `print()`, `summary()`, and `plot()`.
#'
#' @returns A `epicdata_meta` object. Its structure is described in
#'   Details.
#'
#' @rdname epicdata_meta
#'
#' @export
#'
#' @examples
#' x <- list(data.frame(a = c(1,2,3), b = c(4,5,6)),
#' data.frame(a = c("a","b","c"), b = c("d","e","f")))
#' metadata <- new_meta(x)
new_meta <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "epicdata_meta")
}

#' @description `new_meta()` and `validate_meta()` define the S3 class.
#'
#' @rdname epicdata_meta
#'
#' @export
#'
validate_meta <- function(x) {

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

#' @description `print()` prints
#'
#' @method print epicdata_meta
#'
#' @rdname epicdata_meta
#'
#' @export
#'
print.epicdata_meta <- function(x, ...) {

  # cat("The Metadata describes ",nrow(x$main)," variables.\n\n")

}

read_meta <- function(file) {
  # Check input


  #=============================================================================
  # Read in YAML file
  yaml_input <- meta.read(file)

  # Check structure after reading-in the file
  meta.str.input(yaml_input)
  yaml_input %<>% meta.forgive.component.name()
  #meta.str.component(yaml_input)
  meta.str.options(yaml_input)

  # # Process read-in file
  #
  # ## Process var.list fields cats and cats.eng
  # x %<>% yaml.prc.var.list.cats("cats")
  # x %<>% yaml.prc.var.list.cats("cats.eng")
  #
  # # Transform corresponding elements to data.frames
  # x %<>% purrr::map(\(x) if (is.list(x)) {
  #   purrr::map_dfr(x, as.data.frame)
  # } else {
  #   as.character(x)
  # })
  #
  # # Transform to epicdata_meta
  # x %<>% new_metadata()
  #
  # # Validate
  # x %<>% validate_metadata()

  return(yaml_input)
}
