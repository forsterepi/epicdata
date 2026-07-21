#' Title
#'
#' @param file
#' @param include_schema
#' @param open
#'
#' @returns
#'
#' @export
#' @examples
create_yaml <- function(file, include_schema = FALSE, open = TRUE) {
  # Check inputs
  rlang::check_installed("fs")

  rlang::try_fetch(
    {
      checkmate::assert_path_for_output(file, overwrite = FALSE)
      checkmate::assert_flag(include_schema, na.ok = FALSE, null.ok = FALSE)
      checkmate::assert_flag(open, na.ok = FALSE, null.ok = FALSE)
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "x" = "Invalid input for function `create_yaml()`",
          "i" = cnd$message
        ),
        class = "error.create_yaml.1",
        parent = NA
      )
    }
  )

  # Check extension
  if (
    stringi::stri_detect_regex(
      str = file,
      pattern = ".\\.yml$|.\\.yaml$",
      negate = TRUE
    )
  ) {
    cli::cli_abort(
      c(
        "x" = "Invalid input for function `create_yaml()`",
        "i" = "'file' must have extension `.yaml` or `.yml`."
      ),
      class = "error.create_yaml.2"
    )
  }

  # Create file
  rlang::try_fetch(
    fs::file_create(path = file),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "x" = "Cannot create file {.file {file}}"
        ),
        class = "error.create_yaml.3",
        parent = NA
      )
    }
  )

  # Write to file
  lines <- c(
    "options:",
    "  data.name: my_data",
    "var.list:",
    "  x:",
    "    type: text"
  )

  if (include_schema) {
    schema_path <- rlang::try_fetch(
      system.file(
        "epicdata-schema.json",
        package = "epicdata",
        mustWork = TRUE
      ),
      error = function(cnd) {
        cli::cli_abort(
          "Internal error (ID: error.create_yaml.1)",
          class = "error.create_yaml.4",
          parent = NA,
          .internal = TRUE
        )
      }
    )

    lines <- c(
      paste0(
        "# yaml-language-server: $schema=",
        fs::path_rel(
          path = schema_path,
          start = file
        )
      ),
      "",
      lines
    )
  }

  writeLines(lines, con = file)

  if (open) {
    fs::file_show(file)
  }
}
