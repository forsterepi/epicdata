test_that("property study.name works", {
  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
      "options:",
      "  study.name: test",
      "var.list:",
      "  id:",
      "    type: text"
    )
  )

  m <- metadata(file)
  expect_equal(m@study.name, "test")

  m@study.name <- NULL
  expect_equal(m@study.name, "my_study")

  m@study.name <- "name"
  expect_equal(m@study.name, "name")

  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "var.list:",
    "  id:",
    "    type: text"
    )
  )

  m <- metadata(file)
  expect_equal(m@study.name, "my_study")
})

test_that("properties id.var, DUP_NO.ID, and DUP_FREQ works", {
  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "  study.name: test",
    "var.list:",
    "  id:",
    "    type: text"
    )
  )

  m <- metadata(file)
  expect_equal(m@id.var, NULL)
  expect_true(m@DUP_NO.ID)
  expect_false(m@DUP_FREQ)

  m@id.var <- "id"
  expect_equal(m@id.var, "id")
  expect_false(m@DUP_NO.ID)
  expect_true(m@DUP_FREQ)

  expect_snapshot(error = TRUE, m@id.var <- c("a","b"))
  expect_snapshot(error = TRUE, m@id.var <- "a")

  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "  study.name: test",
    "  id.var: id",
    "var.list:",
    "  id:",
    "    type: text"
    )
  )

  m <- metadata(file)
  expect_equal(m@id.var, "id")
  expect_false(m@DUP_NO.ID)
  expect_true(m@DUP_FREQ)

  m@id.var <- NULL
  expect_equal(m@id.var, NULL)
  expect_true(m@DUP_NO.ID)
  expect_false(m@DUP_FREQ)

  expect_snapshot(error = TRUE, m@DUP_NO.ID <- FALSE)
  expect_snapshot(error = TRUE, m@DUP_FREQ <- TRUE)
})

test_that("touch.na works", {

  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "  touch.na:",
    "var.list:",
    "  b:",
    "    type: text",
    "  a:",
    "    type: text",
    "    group: g",
    "var.groups:",
    "  g:",
    "    touch.na:"
    )
  )

  m <- metadata(file)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@touch.na <- FALSE
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@touch.na <- TRUE
  expect_equal(m@touch.na, TRUE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], TRUE)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@touch.na <- FALSE
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@touch.na <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- TRUE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], TRUE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$group <- "h"
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$h$group.name <- "h"
  m@var.groups$h$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$group <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$group <- "g"
  m@var.list$b$group <- "g"
  m@var.groups$h <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@var.groups$g$group.name <- "h"
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$h[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$h$group.name <- "g"
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@var.groups$g <- NULL
  expect_equal(m@var.groups, NULL)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$h[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m <- metadata(file)
  m@var.groups$g$touch.na <- FALSE
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups <- NULL
  expect_equal(m@var.groups, NULL)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m <- metadata(file)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], FALSE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- TRUE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], TRUE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], FALSE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  # Load file with full specification and check if all works correctly

  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "  touch.na: no",
    "var.list:",
    "  a:",
    "    type: text",
    "    group: g",
    "    touch.na: yes",
    "  b:",
    "    type: text",
    "    group: h",
    "var.groups:",
    "  g:",
    "    touch.na: no",
    "  h:",
    "    touch.na: yes"
    )
  )

  m <- metadata(file)
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], TRUE)
  expect_equal(m@var.list$a[["touch.na"]], TRUE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], TRUE)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  # touch.na.default.option cannot be changed directly

  expect_snapshot(error = TRUE, m@var.list$b$touch.na.default.option <- TRUE)
  expect_snapshot(error = TRUE, m@var.list$b$touch.na.default.option <- NULL)
  expect_no_error(m@var.list$b$touch.na.default.option <- FALSE)

  m@touch.na <- NULL
  expect_snapshot(error = TRUE, m@var.list$b$touch.na.default.option <- TRUE)

  # touch.na.default.group cannot be changed directly

  expect_snapshot(error = TRUE, m@var.list$b$touch.na.default.group <- FALSE)
  expect_snapshot(error = TRUE, m@var.list$b$touch.na.default.group <- NULL)
  expect_no_error(m@var.list$b$touch.na.default.group <- TRUE)

  m@var.groups$h$touch.na <- NULL
  expect_snapshot(error = TRUE, m@var.list$b$touch.na.default.group <- TRUE)

  m@var.list$b$group <- NULL
  expect_snapshot(error = TRUE, m@var.list$b$touch.na.default.group <- TRUE)

  # touch.na.final cannot be changed directly

  expect_equal(m@var.list$b$touch.na.final, TRUE)
  expect_no_error(m@var.list$b$touch.na.final <- FALSE)
  expect_equal(m@var.list$b$touch.na.final, TRUE)
  expect_no_error(m@var.list$b$touch.na.final <- NULL)
  expect_equal(m@var.list$b$touch.na.final, TRUE)

  m@touch.na <- FALSE
  expect_equal(m@var.list$b$touch.na.final, FALSE)
  expect_no_error(m@var.list$b$touch.na.final <- TRUE)
  expect_equal(m@var.list$b$touch.na.final, FALSE)
  expect_no_error(m@var.list$b$touch.na.final <- NULL)
  expect_equal(m@var.list$b$touch.na.final, FALSE)

  m@touch.na <- NULL
  expect_equal(m@var.list$b$touch.na.final, TRUE)
  m@var.list$b$group <- "g"
  expect_equal(m@var.list$b$touch.na.final, FALSE)
  expect_no_error(m@var.list$b$touch.na.final <- TRUE)
  expect_equal(m@var.list$b$touch.na.final, FALSE)
  expect_no_error(m@var.list$b$touch.na.final <- NULL)
  expect_equal(m@var.list$b$touch.na.final, FALSE)

  m@var.list$b$group <- NULL
  expect_equal(m@var.list$b$touch.na.final, TRUE)
  m@var.list$b$touch.na <- FALSE
  expect_equal(m@var.list$b$touch.na.final, FALSE)
  expect_no_error(m@var.list$b$touch.na.final <- TRUE)
  expect_equal(m@var.list$b$touch.na.final, FALSE)
  expect_no_error(m@var.list$b$touch.na.final <- NULL)
  expect_equal(m@var.list$b$touch.na.final, FALSE)
})

# add tests for id.pattern
# add tests for all the validations
# add tests for errors in mate.prop.var.list & meta.prop.var.groups
