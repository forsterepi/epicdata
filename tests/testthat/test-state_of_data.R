test_that("all.vars.character works", {
  file <- withr::local_tempfile(
    pattern = "test",
    fileext = "yml",
    lines = c(
      "var.list:",
      "  a:",
      "    type: text",
      "  b:",
      "    type: text"
    )
  )
  m <- metadata(file)

  # Check input class
  df <- data.frame(
    a = c("a", "b", "c"),
    b = c("a", "b", NA)
  )

  expect_true(checkmate::test_class(df, classes = c("data.frame")))
  expect_false(checkmate::test_class(df, classes = c("data.table")))
  expect_false(checkmate::test_class(df, classes = c("tbl_df")))

  dt <- df %>%
    data.table::data.table()

  expect_true(checkmate::test_class(dt, classes = c("data.frame")))
  expect_true(checkmate::test_class(dt, classes = c("data.table")))
  expect_false(checkmate::test_class(dt, classes = c("tbl_df")))

  ti <- df %>%
    tibble::as_tibble()

  expect_true(checkmate::test_class(ti, classes = c("data.frame")))
  expect_false(checkmate::test_class(ti, classes = c("data.table")))
  expect_true(checkmate::test_class(ti, classes = c("tbl_df")))

  ## Character ----
  expect_no_error(state.of.the.data(
    df,
    metadata = m,
    when = "before",
    all.vars.character = TRUE
  ))
  expect_no_error(state.of.the.data(
    dt,
    metadata = m,
    when = "before",
    all.vars.character = TRUE
  ))
  expect_no_error(state.of.the.data(
    ti,
    metadata = m,
    when = "before",
    all.vars.character = TRUE
  ))
  expect_no_error(state.of.the.data(
    df,
    metadata = m,
    when = "after",
    all.vars.character = TRUE
  ))
  expect_no_error(state.of.the.data(
    dt,
    metadata = m,
    when = "after",
    all.vars.character = TRUE
  ))
  expect_no_error(state.of.the.data(
    ti,
    metadata = m,
    when = "after",
    all.vars.character = TRUE
  ))

  ## Double ----
  df <- data.frame(
    a = c("a", "b", "c"),
    b = c(1.1, 2, 3)
  )
  dt <- df %>%
    data.table::data.table()
  ti <- df %>%
    tibble::as_tibble()

  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )

  ## Integer ----
  df <- data.frame(
    a = c("a", "b", "c"),
    b = c(1L, 2L, 3L)
  )
  dt <- df %>%
    data.table::data.table()
  ti <- df %>%
    tibble::as_tibble()

  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )

  ## Factor ----
  df <- data.frame(
    a = c("a", "b", "c"),
    b = c("a", "b", "c") %>% as.factor()
  )
  dt <- df %>%
    data.table::data.table()
  ti <- df %>%
    tibble::as_tibble()

  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )

  ## Logical ----
  df <- data.frame(
    a = c("a", "b", "c"),
    b = c(TRUE, TRUE, FALSE)
  )
  dt <- df %>%
    data.table::data.table()
  ti <- df %>%
    tibble::as_tibble()

  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )

  ## haven::labelled ----
  df <- data.frame(
    a = c("a", "b", "c"),
    b = haven::labelled(
      c("M", "F", "F"),
      c(Male = "M", Female = "F")
    )
  )
  dt <- df %>%
    data.table::data.table()
  ti <- df %>%
    tibble::as_tibble()

  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "before",
      all.vars.character = TRUE
    ),
    class = "error.state.of.the.data.1"
  )
  expect_error(
    state.of.the.data(
      df,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      dt,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
  expect_error(
    state.of.the.data(
      ti,
      metadata = m,
      when = "after",
      all.vars.character = TRUE
    ),
    class = "IE21111"
  )
})
