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

test_that("property consent works", {
  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "  id.var:",
    "  consent:",
    "var.list:",
    "  id:",
    "    type: text"
    )
  )

  m <- metadata(file)
  expect_null(m@id.var)
  expect_null(m@consent)
  expect_false(m@consent.final)

  expect_snapshot(error = TRUE, m@consent.final <- TRUE)
  expect_snapshot(error = TRUE, m@consent <- TRUE)

  m@id.var <- "id"
  expect_equal(m@id.var, "id")
  expect_null(m@consent)
  expect_true(m@consent.final)

  m@id.var <- NULL
  expect_null(m@id.var)
  expect_null(m@consent)
  expect_false(m@consent.final)

  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "  id.var: id",
    "  consent: no",
    "var.list:",
    "  id:",
    "    type: text"
    )
  )

  m <- metadata(file)
  expect_equal(m@id.var, "id")
  expect_false(m@consent)
  expect_false(m@consent.final)

  m@consent <- NULL
  expect_equal(m@id.var, "id")
  expect_null(m@consent)
  expect_true(m@consent.final)

  m@consent <- FALSE
  expect_equal(m@id.var, "id")
  expect_false(m@consent)
  expect_false(m@consent.final)

  m@consent <- TRUE
  expect_equal(m@id.var, "id")
  expect_true(m@consent)
  expect_true(m@consent.final)

  expect_snapshot(error = TRUE, m@id.var <- NULL)
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
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@touch.na <- FALSE
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@na.touch, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@touch.na <- TRUE
  expect_equal(m@touch.na, TRUE)
  expect_equal(m@na.touch, TRUE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], TRUE)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@touch.na <- FALSE
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@na.touch, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@touch.na <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- TRUE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], TRUE)
  expect_equal(m@var.groups$g[["na.touch"]], TRUE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$group <- "h"
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$h$group.name <- "h"
  m@var.groups$h$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$group <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
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
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], NULL)
  expect_equal(m@var.groups$h[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@var.groups$g$group.name <- "h"
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.groups$h[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$h$group.name <- "g"
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], NULL)
  expect_equal(m@var.groups$h[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@var.groups$g <- NULL
  expect_equal(m@var.groups, NULL)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.groups$h[["touch.na"]], NULL)
  expect_equal(m@var.groups$h[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m <- metadata(file)
  m@var.groups$g$touch.na <- FALSE
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups <- NULL
  expect_equal(m@var.groups, NULL)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m <- metadata(file)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["na.touch"]], FALSE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- TRUE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], TRUE)
  expect_equal(m@var.list$a[["na.touch"]], TRUE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["na.touch"]], FALSE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$touch.na <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
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
  expect_equal(m@na.touch, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], TRUE)
  expect_equal(m@var.groups$h[["na.touch"]], TRUE)
  expect_equal(m@var.list$a[["touch.na"]], TRUE)
  expect_equal(m@var.list$a[["na.touch"]], TRUE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
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

  # Set to wrong format
  m <- metadata(file)
  expect_snapshot(error = TRUE, m@touch.na <- 3)
  expect_snapshot(error = TRUE, m@touch.na <- NA)
  expect_snapshot(error = TRUE, m@touch.na <- c(TRUE, FALSE))
  expect_snapshot(error = TRUE, m@touch.na <- c(TRUE, NA))
  expect_snapshot(error = TRUE, m@touch.na <- "TRUE")
  expect_snapshot(error = TRUE, m@na.touch <- 3)
  expect_snapshot(error = TRUE, m@na.touch <- NA)
  expect_snapshot(error = TRUE, m@na.touch <- c(TRUE, FALSE))
  expect_snapshot(error = TRUE, m@na.touch <- c(TRUE, NA))
  expect_snapshot(error = TRUE, m@na.touch <- "TRUE")

  expect_snapshot(error = TRUE, m@var.list$a$touch.na <- 3)
  expect_snapshot(error = TRUE, m@var.list$a$touch.na <- NA)
  expect_snapshot(error = TRUE, m@var.list$a$touch.na <- c(TRUE, FALSE))
  expect_snapshot(error = TRUE, m@var.list$a$touch.na <- c(TRUE, NA))
  expect_snapshot(error = TRUE, m@var.list$a$touch.na <- "TRUE")
  expect_snapshot(error = TRUE, m@var.list$a$na.touch <- 3)
  expect_snapshot(error = TRUE, m@var.list$a$na.touch <- NA)
  expect_snapshot(error = TRUE, m@var.list$a$na.touch <- c(TRUE, FALSE))
  expect_snapshot(error = TRUE, m@var.list$a$na.touch <- c(TRUE, NA))
  expect_snapshot(error = TRUE, m@var.list$a$na.touch <- "TRUE")

  expect_snapshot(error = TRUE, m@var.groups$g$touch.na <- 3)
  expect_snapshot(error = TRUE, m@var.groups$g$touch.na <- NA)
  expect_snapshot(error = TRUE, m@var.groups$g$touch.na <- c(TRUE, FALSE))
  expect_snapshot(error = TRUE, m@var.groups$g$touch.na <- c(TRUE, NA))
  expect_snapshot(error = TRUE, m@var.groups$g$touch.na <- "TRUE")
  expect_snapshot(error = TRUE, m@var.groups$g$na.touch <- 3)
  expect_snapshot(error = TRUE, m@var.groups$g$na.touch <- NA)
  expect_snapshot(error = TRUE, m@var.groups$g$na.touch <- c(TRUE, FALSE))
  expect_snapshot(error = TRUE, m@var.groups$g$na.touch <- c(TRUE, NA))
  expect_snapshot(error = TRUE, m@var.groups$g$na.touch <- "TRUE")

  # Try alternative values

  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
                                lines = c(
                                  "options:",
                                  "  na.touch:",
                                  "var.list:",
                                  "  b:",
                                  "    type: text",
                                  "  a:",
                                  "    type: text",
                                  "    group: g",
                                  "var.groups:",
                                  "  g:",
                                  "    na.touch:"
                                )
  )

  m <- metadata(file)
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@na.touch <- FALSE
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@na.touch, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@na.touch <- TRUE
  expect_equal(m@touch.na, TRUE)
  expect_equal(m@na.touch, TRUE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], TRUE)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@na.touch <- FALSE
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@na.touch, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], FALSE)

  m@na.touch <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$na.touch <- TRUE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], TRUE)
  expect_equal(m@var.groups$g[["na.touch"]], TRUE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$na.touch <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$na.touch <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.groups$g$na.touch <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$group <- "h"
  m@var.groups$h$group.name <- "h"
  m@var.groups$h$na.touch <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], FALSE)
  expect_equal(m@var.groups$h[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m <- metadata(file)
  m@var.groups$g$na.touch <- FALSE
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m <- metadata(file)
  m@var.list$a$na.touch <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["na.touch"]], FALSE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$na.touch <- TRUE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], TRUE)
  expect_equal(m@var.list$a[["na.touch"]], TRUE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$na.touch <- FALSE
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], FALSE)
  expect_equal(m@var.list$a[["na.touch"]], FALSE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.group"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$b[["touch.na.default.option"]], NULL)
  expect_equal(m@var.list$a[["touch.na.final"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)

  m@var.list$a$na.touch <- NULL
  expect_equal(m@touch.na, NULL)
  expect_equal(m@na.touch, NULL)
  expect_equal(m@var.groups$g[["touch.na"]], NULL)
  expect_equal(m@var.groups$g[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na"]], NULL)
  expect_equal(m@var.list$a[["na.touch"]], NULL)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
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
                                  "  na.touch: no",
                                  "var.list:",
                                  "  a:",
                                  "    type: text",
                                  "    group: g",
                                  "    na.touch: yes",
                                  "  b:",
                                  "    type: text",
                                  "    group: h",
                                  "var.groups:",
                                  "  g:",
                                  "    na.touch: no",
                                  "  h:",
                                  "    na.touch: yes"
                                )
  )

  m <- metadata(file)
  expect_equal(m@touch.na, FALSE)
  expect_equal(m@na.touch, FALSE)
  expect_equal(m@var.groups$g[["touch.na"]], FALSE)
  expect_equal(m@var.groups$g[["na.touch"]], FALSE)
  expect_equal(m@var.groups$h[["touch.na"]], TRUE)
  expect_equal(m@var.groups$h[["na.touch"]], TRUE)
  expect_equal(m@var.list$a[["touch.na"]], TRUE)
  expect_equal(m@var.list$a[["na.touch"]], TRUE)
  expect_equal(m@var.list$b[["touch.na"]], NULL)
  expect_equal(m@var.list$b[["na.touch"]], NULL)
  expect_equal(m@var.list$a[["touch.na.default.group"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.group"]], TRUE)
  expect_equal(m@var.list$a[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$b[["touch.na.default.option"]], FALSE)
  expect_equal(m@var.list$a[["touch.na.final"]], TRUE)
  expect_equal(m@var.list$b[["touch.na.final"]], TRUE)
})

# add tests for id.pattern
# add tests for all the validations
# add tests for errors in mate.prop.var.list & meta.prop.var.groups
