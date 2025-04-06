test_that("setter.variable.cats works", {
  expect_equal(setter.variable.cats(cats = NULL, name = "var"), NULL)
  expect_error(setter.variable.cats(cats = NA, name = "var"),
               class = "error.setter.variable.cats.1")

  cats <- c("1=female", NA)
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.1")

  expect_error(setter.variable.cats(cats = 3, name = "var"),
               class = "error.setter.variable.cats.2")

  expect_error(setter.variable.cats(cats = "", name = "var"),
               class = "error.setter.variable.cats.3")

  cats <- c("0==male", "1=female")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.3")

  cats <- c(male = "0", female = "1")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.3")

  cats <- c("1.3 = male", "1 = female")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.4")

  cats <- c("a=male", "1=female")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.5")

  cats <- c("0=1", "2=female")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.6")

  cats <- c("-1 = male", "-2 =male")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.7")

  cats <- c("1 = male", "1 = female")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.8")

  cats <- c("0 = male", "1 =male")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.9")

  cats <- c("0 = male")
  expect_error(setter.variable.cats(cats = cats, name = "var"),
               class = "error.setter.variable.cats.10")

  out <- data.frame(level = c(0L, 1L), label = c("no", "yes"))
  cats <- c("0= no", "yes=1 ")
  expect_equal(setter.variable.cats(cats = cats, name = "var"), out)

  cats <- c("no= 0", "yes=1 ")
  expect_equal(setter.variable.cats(cats = cats, name = "var"), out)

  cats <- c("yes=1 ", "no= 0")
  expect_equal(setter.variable.cats(cats = cats, name = "var"), out)
})
