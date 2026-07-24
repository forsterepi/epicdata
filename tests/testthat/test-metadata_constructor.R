test_that("regex in json schema works", {
  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  id.var: .3a",
      "var.list:",
      "  a:",
      "    type: text"
    )
  )

  testthat::skip("error messages need to be finalised before re-snapshotting")

  expect_snapshot(metadata(file), error = TRUE)

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  id.var: _a",
      "var.list:",
      "  a:",
      "    type: text"
    )
  )

  expect_snapshot(metadata(file), error = TRUE)

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  id.var: a-a",
      "var.list:",
      "  a:",
      "    type: text"
    )
  )

  expect_snapshot(metadata(file), error = TRUE)

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  id.var: aZBy3_._",
      "var.list:",
      "  aZBy3_._:",
      "    type: text"
    )
  )
  expect_no_error(metadata(file))

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  id.var: Z",
      "var.list:",
      "  Z:",
      "    type: text"
    )
  )
  expect_no_error(metadata(file))

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  id.var: .test",
      "var.list:",
      "  .test:",
      "    type: text"
    )
  )
  expect_no_error(metadata(file))

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  id.var: ..test",
      "var.list:",
      "  ..test:",
      "    type: text"
    )
  )
  expect_no_error(metadata(file))
})
