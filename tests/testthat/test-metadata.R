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

# add tests for id.pattern
