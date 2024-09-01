#' Create empty package
#'
#' Helper function to create empty R project and corresponding folders. We use
#' usethis::create_project(), which creates an empty sub-directory called 'R'.
#' This, from now on, replaces subdirectory 'Scripts'.
#'
#' @param name The name of the project as a single character element. It is
#'   recommended to only use lower case letters, numbers, as well as _.
#' @param path The path where the project should be created, without the name of
#'   the project.
#'
#' @noRd
create_new_project <- function(name, path) {
  usethis::create_project(file.path(path, name), rstudio = T, open = F)
  dir.create(file.path(path, name, "Input"))
  dir.create(file.path(path, name, "Output"))
}

#' Create file based on template
#'
#' Helper function to create an .R file and fill it with code from a template.
#'
#' @param path The path where the project should be created, without the name of
#'   the project.
#' @param project_name The name of the project as a single character element. It
#'   is recommended to only use lower case letters, numbers, as well as _.
#' @param folder The subfolder within the R project, where the .R file should be
#'   saved. Usually 'R' or '' for the top level.
#' @param script_name The name of the script to save. Do not forget the file
#'   extension '.R'.
#' @param template The name of the code template. Templates need to be stored in
#'   inst/script_templates. Do not forget the file extension '.R'.
#'
#' @noRd
create_new_file <- function(path, project_name, folder, script_name, template) {
  checkmate::assert_character(script_name, len = 1, pattern = "\\.R$")
  checkmate::assert_character(template, len = 1, pattern = "\\.R$")

  new_file <- file.path(path, project_name, folder, script_name)
  file.create(new_file)

  template_code <- readLines(system.file("script_templates", template, package = "epicdata"))
  template_code <- gsub(pattern = "<<add_package_title>>", replacement = project_name, x = template_code)
  writeLines(template_code, con = new_file)
}

#' Creating a meta project
#'
#' Creates a project that sets up meta data
#'
#' @param name The name of the project as a single character element. It is
#'   recommended to only use lower case letters, numbers, as well as _ and to
#'   end the name with '_meta'.
#' @param path The path where the project should be created, without the name of
#'   the project.
#' @param db A TRUE/FALSE value indicating if the created project should include
#'   code that supports connection to a data base via DSN in R Studio. The default
#'   is FALSE.
#'
#' @return Creates an R project in the specified library and adds all templates
#'   and structre needed for creating meta data.
#' @export
#'
#' @examples
#' withr::with_tempdir({
#' path <- getwd()
#' create_meta("meta_test", path, db = TRUE)
#' })
create_meta <- function(name, path, db = F) {

  # Check
  rlang::try_fetch(checkmate::assert_character(path, len = 1, min.chars = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var path} must be a single element of type character!",
                                      "i" = "{.var NA}, {.var NULL}, and empty elements are not allowed."),
                                    class = "inpur_error_path", parent = cnd)
                   }
  )

  rlang::try_fetch(checkmate::assert_character(name, len = 1, min.chars = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var name} must be a single element of type character!",
                                      "i" = "{.var NA}, {.var NULL}, and empty elements are not allowed."),
                                    class = "inpur_error_name", parent = cnd)
                   }
  )

  rlang::try_fetch(checkmate::assert_logical(db, len = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var db} must be TRUE or FALSE!",
                                      "i" = "{.var NA} and {.var NULL} are not allowed."),
                                    class = "inpur_error_db", parent = cnd)
                   }
  )

  dir_exists <- dir.exists(path)
  if (!dir_exists) {
    cli::cli_abort("The path does not exist!", class = "dir_exists")
  }

  proj_exists <- dir.exists(file.path(path, name))
  if (proj_exists) {
    cli::cli_abort("The project already exists!", class = "proj_exists")
  }

  # Do
  create_new_project(name = name, path = path)

  create_new_file(path, name, "", "test_workflow.R", "test_template.R")
  create_new_file(path, name, "R", "test_script.R", "test_template.R")

  if (db) {

  }

  cli::cli_alert_success("R project {.emph {name}} was successfully created in {.emph {path}}.")
}

#' Creating a raw project
#'
#' Creates a project that pre-processes raw data
#'
#' @param name The name of the project as a single character element. It is
#'   recommended to only use lower case letters, numbers, as well as _ and to
#'   end the name with '_raw'.
#' @param path The path where the project should be created, without the name of
#'   the project.
#' @param db A TRUE/FALSE value indicating if the created project should include
#'   code that supports connection to a data base via DSN in R Studio. The default
#'   is FALSE.
#'
#' @return Creates an R project in the specified library and adds all templates
#'   and structre needed for pre-processing raw data.
#' @export
#'
#' @examples
#' withr::with_tempdir({
#' path <- getwd()
#' create_raw("raw_test", path, db = TRUE)
#' })
create_raw <- function(name, path, db = F) {

  # Check
  rlang::try_fetch(checkmate::assert_character(path, len = 1, min.chars = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var path} must be a single element of type character!",
                                      "i" = "{.var NA}, {.var NULL}, and empty elements are not allowed."),
                                    class = "inpur_error_path", parent = cnd)
                   }
  )

  rlang::try_fetch(checkmate::assert_character(name, len = 1, min.chars = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var name} must be a single element of type character!",
                                      "i" = "{.var NA}, {.var NULL}, and empty elements are not allowed."),
                                    class = "inpur_error_name", parent = cnd)
                   }
  )

  rlang::try_fetch(checkmate::assert_logical(db, len = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var db} must be TRUE or FALSE!",
                                      "i" = "{.var NA} and {.var NULL} are not allowed."),
                                    class = "inpur_error_db", parent = cnd)
                   }
  )

  dir_exists <- dir.exists(path)
  if (!dir_exists) {
    cli::cli_abort("The path does not exist!", class = "dir_exists")
  }

  proj_exists <- dir.exists(file.path(path, name))
  if (proj_exists) {
    cli::cli_abort("The project already exists!", class = "proj_exists")
  }

  # Do
  create_new_project(name = name, path = path)

  create_new_file(path, name, "", "test_workflow.R", "test_template.R")
  create_new_file(path, name, "R", "test_script.R", "test_template.R")

  if (db) {

  }

  cli::cli_alert_success("R project {.emph {name}} was successfully created in {.emph {path}}.")
}

#' Creating a prc project
#'
#' Creates a project that processes the data
#'
#' @param name The name of the project as a single character element. It is
#'   recommended to only use lower case letters, numbers, as well as _ and to
#'   end the name with '_prc'.
#' @param path The path where the project should be created, without the name of
#'   the project.
#' @param db A TRUE/FALSE value indicating if the created project should include
#'   code that supports connection to a data base via DSN in R Studio. The default
#'   is FALSE.
#'
#' @return Creates an R project in the specified library and adds all templates
#'   and structre needed for processing data.
#' @export
#'
#' @examples
#' withr::with_tempdir({
#' path <- getwd()
#' create_prc("prc_test", path, db = TRUE)
#' })
create_prc <- function(name, path, db = F) {

  # Check
  rlang::try_fetch(checkmate::assert_character(path, len = 1, min.chars = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var path} must be a single element of type character!",
                                      "i" = "{.var NA}, {.var NULL}, and empty elements are not allowed."),
                                    class = "inpur_error_path", parent = cnd)
                   }
  )

  rlang::try_fetch(checkmate::assert_character(name, len = 1, min.chars = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var name} must be a single element of type character!",
                                      "i" = "{.var NA}, {.var NULL}, and empty elements are not allowed."),
                                    class = "inpur_error_name", parent = cnd)
                   }
  )

  rlang::try_fetch(checkmate::assert_logical(db, len = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort(c("Argument {.var db} must be TRUE or FALSE!",
                                      "i" = "{.var NA} and {.var NULL} are not allowed."),
                                    class = "inpur_error_db", parent = cnd)
                   }
  )

  dir_exists <- dir.exists(path)
  if (!dir_exists) {
    cli::cli_abort("The path does not exist!", class = "dir_exists")
  }

  proj_exists <- dir.exists(file.path(path, name))
  if (proj_exists) {
    cli::cli_abort("The project already exists!", class = "proj_exists")
  }

  # Do
  create_new_project(name = name, path = path)

  create_new_file(path, name, "", "test_workflow.R", "test_template.R")
  create_new_file(path, name, "R", "test_script.R", "test_template.R")

  if (db) {

  }

  cli::cli_alert_success("R project {.emph {name}} was successfully created in {.emph {path}}.")
}
