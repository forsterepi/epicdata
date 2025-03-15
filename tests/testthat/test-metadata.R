test_that("setting and getting of metadata@study.name works", {
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
