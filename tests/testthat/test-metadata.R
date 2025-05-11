test_that("property data.name works", {
  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
      "options:",
      "  data.name: test",
      "var.list:",
      "  id:",
      "    type: text"
    )
  )

  m <- metadata(file)
  expect_equal(m@data.name, "test")

  m@data.name <- NULL
  expect_equal(m@data.name, "my_data")

  m@data.name <- "name"
  expect_equal(m@data.name, "name")

  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "var.list:",
    "  id:",
    "    type: text"
    )
  )

  m <- metadata(file)
  expect_equal(m@data.name, "my_data")
})

test_that("properties id.var, DUP_NO.ID, and DUP_FREQ works", {
  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "options:",
    "  data.name: test",
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
    "  data.name: test",
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

test_that("cats and cats.eng work",{
  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
                                lines = c(
                                  "var.list:",
                                  "  a:",
                                  "    type: text"
                                )
  )

  m <- metadata(file)

  expect_equal(m@var.list$a$cats, NULL)
  expect_equal(m@var.list$a$cats.eng, NULL)

  # Add that cats and cats.eng can only be used with type "cat"
  # expect_snapshot(error = TRUE, m@var.list$a$cats <- c("0=nein","1=ja"))
  # expect_snapshot(error = TRUE, m@var.list$a$cats.eng <- c("0=no","1=yes"))

  m@var.list$a$type <- "cat"
  m@var.list$a$cats <- c("0=nein","1=ja")
  out <- data.frame(level = c(0L, 1L), label = c("nein", "ja"))
  expect_equal(m@var.list$a$cats, out)

  # Add that cats.eng must have the same levels as cats
  out.eng <- data.frame(level = c(0L, 1L, 2L), label = c("no", "yes", "maybe"))
  # expect_snapshot(error = TRUE, m@var.list$a$cats.eng <- c("0=no","1=yes","2=maybe"))
  # expect_snapshot(error = TRUE, m@var.list$a$cats.eng <- c("0=no","2=maybe"))

  out.eng2 <- data.frame(level = c(0L, 1L), label = c("no", "yes"))
  m@var.list$a$cats.eng <- c("1=yes","0=no")
  expect_equal(m@var.list$a$cats.eng, out.eng2)

  # Maybe add that cats can only be used for type "cat
  # expect_snapshot(error = TRUE, m@var.list$a$type <- "num")

  # Add that cats.eng must have the same levels as cats
  # expect_snapshot(error = TRUE, m@var.list$a$cats <- c("0=nein","ja=1","2=vielleicht"))
  m@var.list$a$cats.eng <- NULL
  expect_equal(m@var.list$a$cats.eng, NULL)
  m@var.list$a$cats <- c("0=nein","ja=1","2=vielleicht")
  out2 <- data.frame(level = c(0L, 1L, 2L), label = c("nein", "ja", "vielleicht"))
  expect_equal(m@var.list$a$cats, out2)
  m@var.list$a$cats.eng <- c("0=no","1=yes","2=maybe")
  expect_equal(m@var.list$a$cats.eng, out.eng)

  m <- metadata(file)
  m@var.list$a$type <- "cat"
  expect_error(m@var.list$a$cats <- c("1=ja", NA), class = "error.process.cats.1")
  expect_error(m@var.list$a$cats <- NA, class = "error.process.cats.1")
  expect_error(m@var.list$a$cats <- 3, class = "error.process.cats.2")
  expect_error(m@var.list$a$cats <- FALSE, class = "error.process.cats.2")
  expect_error(m@var.list$a$cats <- "", class = "error.process.cats.3")
  expect_error(m@var.list$a$cats <- c("0==male", "1=female"),
               class = "error.process.cats.3")
  expect_error(m@var.list$a$cats <- c(male = "0", female = "1"),
               class = "error.process.cats.3")
  expect_error(m@var.list$a$cats <- c("1.3 = male", "1 = female"),
               class = "error.process.cats.4")
  expect_error(m@var.list$a$cats <- c("a=male", "1=female"),
               class = "error.process.cats.5")
  expect_error(m@var.list$a$cats <- c("0=1", "2=female"),
               class = "error.process.cats.6")
  expect_error(m@var.list$a$cats <- c("-1 = male", "-2 =male"),
               class = "error.process.cats.7")
  expect_error(m@var.list$a$cats <- c("1 = male", "1 = female"),
               class = "error.process.cats.8")
  expect_error(m@var.list$a$cats <- c("0 = male", "1 =male"),
               class = "error.process.cats.9")

  expect_error(m@var.list$a$cats.eng <- c("1=ja", NA), class = "error.process.cats.1")
  expect_error(m@var.list$a$cats.eng <- NA, class = "error.process.cats.1")
  expect_error(m@var.list$a$cats.eng <- 3, class = "error.process.cats.2")
  expect_error(m@var.list$a$cats.eng <- FALSE, class = "error.process.cats.2")
  expect_error(m@var.list$a$cats.eng <- "", class = "error.process.cats.3")
  expect_error(m@var.list$a$cats.eng <- c("0==male", "1=female"),
               class = "error.process.cats.3")
  expect_error(m@var.list$a$cats.eng <- c(male = "0", female = "1"),
               class = "error.process.cats.3")
  expect_error(m@var.list$a$cats.eng <- c("1.3 = male", "1 = female"),
               class = "error.process.cats.4")
  expect_error(m@var.list$a$cats.eng <- c("a=male", "1=female"),
               class = "error.process.cats.5")
  expect_error(m@var.list$a$cats.eng <- c("0=1", "2=female"),
               class = "error.process.cats.6")
  expect_error(m@var.list$a$cats.eng <- c("-1 = male", "-2 =male"),
               class = "error.process.cats.7")
  expect_error(m@var.list$a$cats.eng <- c("1 = male", "1 = female"),
               class = "error.process.cats.8")
  expect_error(m@var.list$a$cats.eng <- c("0 = male", "1 =male"),
               class = "error.process.cats.9")
})

# add tests for id.pattern
# add tests for all the validations
# add tests for errors in mate.prop.var.list & meta.prop.var.groups
