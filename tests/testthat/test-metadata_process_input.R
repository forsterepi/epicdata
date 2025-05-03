test_that("process.cats works", {
  expect_equal(process.cats(cats = NULL, name = "var"), NULL)
  expect_error(process.cats(cats = NA, name = "var"),
               class = "error.process.cats.1")

  cats <- c("1=female", NA)
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.1")

  expect_error(process.cats(cats = 3, name = "var"),
               class = "error.process.cats.2")

  expect_error(process.cats(cats = "", name = "var"),
               class = "error.process.cats.3")

  cats <- c("0==male", "1=female")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.3")

  cats <- c(male = "0", female = "1")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.3")

  cats <- c("1.3 = male", "1 = female")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.4")

  cats <- c("a=male", "1=female")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.5")

  cats <- c("0=1", "2=female")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.6")

  cats <- c("-1 = male", "-2 =male")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.7")

  cats <- c("1 = male", "1 = female")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.8")

  cats <- c("0 = male", "1 =male")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.9")

  cats <- c("0 = male")
  expect_error(process.cats(cats = cats, name = "var"),
               class = "error.process.cats.10")

  out <- data.frame(level = c(0L, 1L), label = c("no", "yes"))
  cats <- c("0= no", "yes=1 ")
  expect_equal(process.cats(cats = cats, name = "var"), out)

  cats <- c("no= 0", "yes=1 ")
  expect_equal(process.cats(cats = cats, name = "var"), out)

  cats <- c("yes=1 ", "no= 0")
  expect_equal(process.cats(cats = cats, name = "var"), out)
})

test_that("check.rules.format works", {
  expect_error(check.rules.format(3), class = "IE006")
  expect_error(check.rules.format(NULL), class = "IE006")
  expect_error(check.rules.format(NA), class = "IE006")
  expect_error(check.rules.format(""), class = "IE006")
  expect_error(check.rules.format(character(0)), class = "IE006")
  expect_error(check.rules.format("var == 1 ~"),
               class = "error.check.rules.format.1")
  expect_error(check.rules.format("~"), class = "error.check.rules.format.1")
  expect_error(check.rules.format("x"), class = "error.check.rules.format.2")
  expect_error(check.rules.format("3"), class = "error.check.rules.format.2")
  expect_error(check.rules.format("ja = yes"),
               class = "error.check.rules.format.3")
  expect_error(check.rules.format("var > 1 ~ NA", var.names = c("x")),
               class = "error.check.rules.format.4")
  expect_error(check.rules.format("var > 1 ~ x", var.names = c("x", "var")),
               class = "error.check.rules.format.6")
  expect_error(check.rules.format("~ x", var.names = c("x", "var")),
               class = "error.check.rules.format.6")
  expect_error(check.rules.format("var > 1 ~ mean(x)", var.names = c("x", "var")),
               class = "error.check.rules.format.6")
  expect_error(check.rules.format("~ mean(x)", var.names = c("x", "var")),
               class = "error.check.rules.format.6")
  expect_error(check.rules.format("var > 1 ~ NULL", var.names = c("x", "var")),
               class = "error.check.rules.format.7")
  expect_error(check.rules.format("~ NULL", var.names = c("x", "var")),
               class = "error.check.rules.format.7")
  expect_error(check.rules.format(c("~ 1", "~ 2")),
               class = "error.check.rules.format.8")

  rule <- c('ctr == "D" ~ "Deutschland"', '~ "Sonstiges"')
  out <- list(rlang::expr(ctr == "D" ~ "Deutschland"),rlang::expr(~ "Sonstiges"))
  expect_equal(check.rules.format(rule, var.names = "ctr"), out)
})

test_that("extract.symbols.from.ast works", {
  expect_equal(extract.symbols.from.ast(NULL), character(0))
  expect_equal(extract.symbols.from.ast(NA), character(0))
  expect_equal(extract.symbols.from.ast("x"), character(0))
  expect_equal(extract.symbols.from.ast(3), character(0))
  expect_equal(extract.symbols.from.ast(TRUE), character(0))
  expect_error(extract.symbols.from.ast(c("1","s")), class = "IE005")
  expect_equal(extract.symbols.from.ast(rlang::expr(c("1","s"))), character(0))
  expect_equal(extract.symbols.from.ast(rlang::expr(x + y)), c("x","y"))
  expect_equal(extract.symbols.from.ast(rlang::expr(~ 3)), character(0))
  expect_equal(extract.symbols.from.ast(rlang::expr(~ NA)), character(0))
  expect_equal(extract.symbols.from.ast(rlang::expr(~ "x")), character(0))
  ex <- rlang::expr(x > 1 & y %in% c("a","b") & base::is.na(z) ~ "4")
  expect_equal(extract.symbols.from.ast(ex), c("x","y","z"))
})
