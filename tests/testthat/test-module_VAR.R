test_that("VAR_DF.NOT.META_NOTE works", {
  file <- withr::local_tempfile(
    pattern = "test", fileext = "yml",
    lines = c(
      "var.list:",
      "  a:",
      "    type: text",
      "  d:",
      "    type: text"
    )
  )

  m <- metadata(file)

  df <- data.frame(
    a = c("a", "b", "c"),
    b = c("a", "b", "c"),
    c = c("a", "b", "c")
  )

  dt <- df %>%
    data.table::data.table()

  ti <- df %>%
    tibble::as_tibble()

  d <- dataset(df, m)
  d2 <- VAR_DF.NOT.META_NOTE(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_NOTE"]], c("b", "c"))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(dt, m)
  d2 <- VAR_DF.NOT.META_NOTE(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_NOTE"]], c("b", "c"))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(ti, m)
  d2 <- VAR_DF.NOT.META_NOTE(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_NOTE"]], c("b", "c"))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  # All vars in meta
  df <- data.frame(
    a = c("a", "b", "c")
  )

  dt <- df %>%
    data.table::data.table()

  ti <- df %>%
    tibble::as_tibble()

  d <- dataset(df, m)
  d2 <- VAR_DF.NOT.META_NOTE(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_NOTE"]], character(0))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(dt, m)
  d2 <- VAR_DF.NOT.META_NOTE(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_NOTE"]], character(0))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(ti, m)
  d2 <- VAR_DF.NOT.META_NOTE(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_NOTE"]], character(0))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)
})

test_that("VAR_DF.NOT.META_RM works", {
  file <- withr::local_tempfile(
    pattern = "test", fileext = "yml",
    lines = c(
      "options:",
      "  remove.vars: true",
      "var.list:",
      "  a:",
      "    type: text",
      "  d:",
      "    type: text"
    )
  )

  m <- metadata(file)

  df <- data.frame(
    a = c("a", "b", "c"),
    b = c("a", "b", "c"),
    c = c("a", "b", "c")
  )
  df_new <- data.frame(
    a = c("a", "b", "c")
  )

  dt <- df %>%
    data.table::data.table()
  dt_new <- df_new %>%
    data.table::data.table()

  ti <- df %>%
    tibble::as_tibble()
  ti_new <- df_new %>%
    tibble::as_tibble()

  d <- dataset(df, m)
  d2 <- VAR_DF.NOT.META_RM(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_RM"]], c("b", "c"))
  expect_equal(d2@processed, df_new)
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(dt, m)
  d2 <- VAR_DF.NOT.META_RM(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_RM"]], c("b", "c"))
  expect_equal(d2@processed, dt_new)
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(ti, m)
  d2 <- VAR_DF.NOT.META_RM(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_RM"]], c("b", "c"))
  expect_equal(d2@processed, ti_new)
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  # All vars in meta
  df <- data.frame(
    a = c("a", "b", "c")
  )

  dt <- df %>%
    data.table::data.table()

  ti <- df %>%
    tibble::as_tibble()

  d <- dataset(df, m)
  d2 <- VAR_DF.NOT.META_RM(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_RM"]], character(0))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(dt, m)
  d2 <- VAR_DF.NOT.META_RM(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_RM"]], character(0))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)

  d <- dataset(ti, m)
  d2 <- VAR_DF.NOT.META_RM(d)

  expect_equal(d2@log[["VAR_DF.NOT.META_RM"]], character(0))
  expect_equal(d2@raw, d@raw)
  expect_equal(d2@processed, d@processed)
  expect_equal(d2@decisions, d@decisions)
  expect_equal(d2@metadata, d@metadata)
})
