#' Check metadata upload file type
#'
#' Checks if the file uploaded into the Metadata Creator `shiny` app is a .rds
#' file.
#'
#' @param x Input of the fileInput UI element, e.g., input$upload.
#'
#' @noRd
check_if_rds <- function(x) {
  req(x)

  ext <- tools::file_ext(x$name)

  if (ext != "rds") {
    shinyalert::shinyalert(title = "Warning!", text = "Please upload an .rds file.", type = "warning")
  }

  validate(
    need(ext == "rds", "Unsupported file type. Please upload an .rds file.")
  )
}

#' Check metadata upload class and structure
#'
#' Checks if the file uploaded into the Metadata Creator `shiny` app has the
#' correct class `epicdata_meta` and the correct structure using
#' `validate_metadata()`.
#'
#' @param x The uploaded file after reading it with `readRDS()`.
#'
#' @noRd
check_if_metadata <- function(x) {
  tryCatch({
    validate_metadata(x)
  }, error = function(e) {
    shinyalert::shinyalert(title = "Warning!", text = "The selected file is an .rds file but does not contain an object of class epicdata_meta.", type = "warning")
    validate(
      need(FALSE, paste("File validation error:", e$message))
    )
  })
}
