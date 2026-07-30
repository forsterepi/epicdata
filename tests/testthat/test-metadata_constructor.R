test_that("json: regex", {
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

test_that("json: errors for empty keys", {
  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options: 3",
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
      "var.list:"
    )
  )
  expect_snapshot(metadata(file), error = TRUE)

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "var.list: 3"
    )
  )
  expect_snapshot(metadata(file), error = TRUE)

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "var.groups:",
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
      "na.codes:",
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
      "import:",
      "var.list:",
      "  a:",
      "    type: text"
    )
  )
  expect_snapshot(metadata(file), error = TRUE)
})

test_that("json: errors for contras", {
  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "var.list:",
      "  a:",
      "    type: text",
      "contras:"
    )
  )
  expect_snapshot(metadata(file), error = TRUE)

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "var.list:",
      "  a:",
      "    type: text",
      "contras:",
      "- true",
      "- 3"
    )
  )
  expect_snapshot(metadata(file), error = TRUE)

  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "var.list:",
      "  a:",
      "    type: text",
      "contras:",
      "- a",
      "- a"
    )
  )
  expect_snapshot(metadata(file), error = TRUE)
})

test_that("json: errors for alias keys", {
  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  remove.vars: true",
      "  vars.remove: true",
      "var.list:",
      "  a:",
      "    type: text"
    )
  )
  expect_snapshot(metadata(file), error = TRUE)
})
