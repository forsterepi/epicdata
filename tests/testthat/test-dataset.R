test_that("S7 class dataset works", {
  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "options:",
      "  data.name: test",
      "var.list:",
      "  a:",
      "    type: text",
      "  b:",
      "    type: num"
    )
  )

  m <- metadata(file)

  df <- data.frame(
    a = c("a", "b", "c"),
    b = c(1, 2, 3)
  )

  dt <- df %>%
    data.table::data.table()

  ti <- df %>%
    tibble::as_tibble()

  expect_no_error(dataset(raw = df, metadata = m))
  expect_no_error(dataset(raw = dt, metadata = m))
  expect_no_error(dataset(raw = ti, metadata = m))

  d <- dataset(raw = df, metadata = m)

  expect_error(dataset(raw = r))
  expect_error(dataset(metadata = m))
  expect_true(identical(d@raw, d@processed))
  expect_equal(d@raw, df)
  expect_equal(d@processed, df)
  expect_equal(d@metadata, m)
  expect_equal(d@decisions, list())
  expect_equal(d@log, list())
})
