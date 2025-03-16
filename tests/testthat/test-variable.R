test_that("property @name works", {
  expect_snapshot(error = TRUE, variable(name = "if", type = "text"))
  expect_snapshot(error = TRUE, variable(type = "text"))
  expect_snapshot(error = TRUE, variable(name = "", type = "text"))
  expect_snapshot(error = TRUE, variable(name = NA, type = "text"))
  expect_snapshot(error = TRUE, variable(name = NULL, type = "text"))
  expect_snapshot(error = TRUE, variable(name = c("a","b"), type = "text"))

  v <- variable(name = "id", type = "text")
  expect_equal(v@name, "id")

  v@name <- "var"
  expect_equal(v@name, "var")

  expect_snapshot(error = TRUE, v@name <- NULL)
})

test_that("property @type works", {
  expect_snapshot(error = TRUE, variable(name = "id"))
  expect_snapshot(error = TRUE, variable(name = "id", type = ""))
  expect_snapshot(error = TRUE, variable(name = "id", type = NA))
  expect_snapshot(error = TRUE, variable(name = "id", type = NULL))
  expect_snapshot(error = TRUE, variable(name = "id", type = c("text","cat")))

  v <- variable(name = "id", type = "text")
  expect_equal(v@type, "text")

  v@type <- "cat"
  expect_equal(v@type, "cat")

  expect_snapshot(error = TRUE, v@type <- NULL)
})

test_that("property @old.id works", {
  v <- variable(name = "id", type = "text")
  expect_equal(v@old.id, NULL)

  v@old.id <- "old"
  expect_equal(v@old.id, "old")

  expect_snapshot(error = TRUE, v@old.id <- "id")
})

test_that("properties @label and @label.eng work", {
  v <- variable(name = "id", type = "text")
  expect_equal(v@label, NULL)
  expect_equal(v@label.eng, NULL)

  expect_snapshot(error = TRUE, v@label.eng <- "the label")

  v@label <- "Der Label"
  expect_equal(v@label, "Der Label")

  v@label.eng <- "the label"
  expect_equal(v@label.eng, "the label")
})

test_that("properties @cats and @cats.eng work",{
  v <- variable(name = "id", type = "text")
  expect_equal(v@cats, NULL)
  expect_equal(v@cats.eng, NULL)

  expect_snapshot(error = TRUE, v@cats <- c("0=nein","1=ja"))
  expect_snapshot(error = TRUE, v@cats.eng <- c("0=no","1=yes"))

  v@type <- "cat"
  v@cats <- c("0=nein","1=ja")
  out <- data.frame(level = c(0L, 1L), label = c("nein", "ja"))
  expect_equal(v@cats, out)

  out.eng <- data.frame(level = c(0L, 1L, 2L), label = c("no", "yes", "maybe"))
  expect_snapshot(error = TRUE, v@cats.eng <- c("0=no","1=yes","2=maybe"))
  expect_snapshot(error = TRUE, v@cats.eng <- c("0=no","2=maybe"))

  out.eng2 <- data.frame(level = c(0L, 1L), label = c("no", "yes"))
  v@cats.eng <- c("1=yes","0=no")
  expect_equal(v@cats.eng, out.eng2)

  expect_snapshot(error = TRUE, v@type <- "num")

  expect_snapshot(error = TRUE, v@cats <- c("0=nein","ja=1","2=vielleicht"))
  v@cats.eng <- NULL
  expect_equal(v@cats.eng, NULL)
  v@cats <- c("0=nein","ja=1","2=vielleicht")
  out2 <- data.frame(level = c(0L, 1L, 2L), label = c("nein", "ja", "vielleicht"))
  expect_equal(v@cats, out2)
  v@cats.eng <- c("0=no","1=yes","2=maybe")
  expect_equal(v@cats.eng, out.eng)

  v <- variable(name = "id", type = "cat")
  expect_error(v@cats <- c("1=ja", NA), class = "error.setter.variable.cats.1")
  expect_error(v@cats <- NA, class = "error.setter.variable.cats.1")
  expect_error(v@cats <- 3, class = "error.setter.variable.cats.2")
  expect_error(v@cats <- FALSE, class = "error.setter.variable.cats.2")
  expect_error(v@cats <- "", class = "error.setter.variable.cats.3")
  expect_error(v@cats <- c("0==male", "1=female"),
               class = "error.setter.variable.cats.3")
  expect_error(v@cats <- c(male = "0", female = "1"),
               class = "error.setter.variable.cats.3")
  expect_error(v@cats <- c("1.3 = male", "1 = female"),
               class = "error.setter.variable.cats.4")
  expect_error(v@cats <- c("a=male", "1=female"),
               class = "error.setter.variable.cats.5")
  expect_error(v@cats <- c("0=1", "2=female"),
               class = "error.setter.variable.cats.6")
  expect_error(v@cats <- c("-1 = male", "-2 =male"),
               class = "error.setter.variable.cats.7")
  expect_error(v@cats <- c("1 = male", "1 = female"),
               class = "error.setter.variable.cats.8")
  expect_error(v@cats <- c("0 = male", "1 =male"),
               class = "error.setter.variable.cats.9")

  expect_error(v@cats.eng <- c("1=ja", NA), class = "error.setter.variable.cats.1")
  expect_error(v@cats.eng <- NA, class = "error.setter.variable.cats.1")
  expect_error(v@cats.eng <- 3, class = "error.setter.variable.cats.2")
  expect_error(v@cats.eng <- FALSE, class = "error.setter.variable.cats.2")
  expect_error(v@cats.eng <- "", class = "error.setter.variable.cats.3")
  expect_error(v@cats.eng <- c("0==male", "1=female"),
               class = "error.setter.variable.cats.3")
  expect_error(v@cats.eng <- c(male = "0", female = "1"),
               class = "error.setter.variable.cats.3")
  expect_error(v@cats.eng <- c("1.3 = male", "1 = female"),
               class = "error.setter.variable.cats.4")
  expect_error(v@cats.eng <- c("a=male", "1=female"),
               class = "error.setter.variable.cats.5")
  expect_error(v@cats.eng <- c("0=1", "2=female"),
               class = "error.setter.variable.cats.6")
  expect_error(v@cats.eng <- c("-1 = male", "-2 =male"),
               class = "error.setter.variable.cats.7")
  expect_error(v@cats.eng <- c("1 = male", "1 = female"),
               class = "error.setter.variable.cats.8")
  expect_error(v@cats.eng <- c("0 = male", "1 =male"),
               class = "error.setter.variable.cats.9")
})
