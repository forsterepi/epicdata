#' @include metadata.R
NULL

dataset.constructor <- function(raw, metadata) {
  # Preprocess raw ???

  S7::new_object(
    S7::S7_object(),
    raw = raw,
    metadata = metadata,
    processed = raw,
    decisions = list(),
    log = list()
  )
}


#' Dataset objects
#'
#' S7 class `epicdata::metadata`
#'
#' @param raw Raw data.frame, data.table, or tibble
#' @param metadata A S7 `epicdata::metadata` object
#'
#' @prop processed A data.frame, data.table, or tibble containing the current
#' state of processing of the `raw` data
#' @prop decisions A list of the decisions taken during data processing
#' @prop log A list logging all changes and other relevant information during
#' data processing
#'
#' @returns A S7 `epicdata::dataset` object
#'
#' @export
#'
dataset <- S7::new_class(
  "dataset",
  properties = list(
    raw = S7::new_property(
      class = S7::class_data.frame,
      default = quote(stop("@raw is required")),
      validator = function(value) {
        # Validate raw ???
      }
    ),
    metadata = S7::new_property(
      class = metadata,
      default = quote(stop("@metadata is required"))
    ),
    processed = S7::new_property(
      class = S7::class_data.frame
    ),
    decisions = S7::new_property(
      class = S7::class_list
    ),
    log = S7::new_property(
      class = S7::class_list
    )
  ),
  constructor = dataset.constructor
)
