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

test_that("cats and cats.eng work in metadata",{
  file <- withr::local_tempfile(pattern = "test", fileext = "yml",
    lines = c(
    "var.list:",
    "  a:",
    "    type: text"
    )
  )

  m <- metadata(file)

  expect_equal(m@var.list$a$cats, NULL)
  expect_equal(m@var.list$a$cats.eng, NULL)

  # Add that cats and cats.eng can only be used with type "cat"
  # expect_snapshot(error = TRUE, m@var.list$a$cats <- c("0=nein","1=ja"))
  # expect_snapshot(error = TRUE, m@var.list$a$cats.eng <- c("0=no","1=yes"))

  m@var.list$a$type <- "cat"
  m@var.list$a$cats <- c("0=nein","1=ja")
  out <- data.frame(level = c(0L, 1L), label = c("nein", "ja"))
  expect_equal(m@var.list$a$cats, out)

  # Add that cats.eng must have the same levels as cats
  out.eng <- data.frame(level = c(0L, 1L, 2L), label = c("no", "yes", "maybe"))
  # expect_snapshot(error = TRUE, m@var.list$a$cats.eng <- c("0=no","1=yes","2=maybe"))
  # expect_snapshot(error = TRUE, m@var.list$a$cats.eng <- c("0=no","2=maybe"))

  out.eng2 <- data.frame(level = c(0L, 1L), label = c("no", "yes"))
  m@var.list$a$cats.eng <- c("1=yes","0=no")
  expect_equal(m@var.list$a$cats.eng, out.eng2)

  # Maybe add that cats can only be used for type "cat
  # expect_snapshot(error = TRUE, m@var.list$a$type <- "num")

  # Add that cats.eng must have the same levels as cats
  # expect_snapshot(error = TRUE, m@var.list$a$cats <- c("0=nein","ja=1","2=vielleicht"))
  m@var.list$a$cats.eng <- NULL
  expect_equal(m@var.list$a$cats.eng, NULL)
  m@var.list$a$cats <- c("0=nein","ja=1","2=vielleicht")
  out2 <- data.frame(level = c(0L, 1L, 2L), label = c("nein", "ja", "vielleicht"))
  expect_equal(m@var.list$a$cats, out2)
  m@var.list$a$cats.eng <- c("0=no","1=yes","2=maybe")
  expect_equal(m@var.list$a$cats.eng, out.eng)

  m <- metadata(file)
  m@var.list$a$type <- "cat"
  expect_error(m@var.list$a$cats <- c("1=ja", NA), class = "error.setter.variable.cats.1")
  expect_error(m@var.list$a$cats <- NA, class = "error.setter.variable.cats.1")
  expect_error(m@var.list$a$cats <- 3, class = "error.setter.variable.cats.2")
  expect_error(m@var.list$a$cats <- FALSE, class = "error.setter.variable.cats.2")
  expect_error(m@var.list$a$cats <- "", class = "error.setter.variable.cats.3")
  expect_error(m@var.list$a$cats <- c("0==male", "1=female"),
               class = "error.setter.variable.cats.3")
  expect_error(m@var.list$a$cats <- c(male = "0", female = "1"),
               class = "error.setter.variable.cats.3")
  expect_error(m@var.list$a$cats <- c("1.3 = male", "1 = female"),
               class = "error.setter.variable.cats.4")
  expect_error(m@var.list$a$cats <- c("a=male", "1=female"),
               class = "error.setter.variable.cats.5")
  expect_error(m@var.list$a$cats <- c("0=1", "2=female"),
               class = "error.setter.variable.cats.6")
  expect_error(m@var.list$a$cats <- c("-1 = male", "-2 =male"),
               class = "error.setter.variable.cats.7")
  expect_error(m@var.list$a$cats <- c("1 = male", "1 = female"),
               class = "error.setter.variable.cats.8")
  expect_error(m@var.list$a$cats <- c("0 = male", "1 =male"),
               class = "error.setter.variable.cats.9")

  expect_error(m@var.list$a$cats.eng <- c("1=ja", NA), class = "error.setter.variable.cats.1")
  expect_error(m@var.list$a$cats.eng <- NA, class = "error.setter.variable.cats.1")
  expect_error(m@var.list$a$cats.eng <- 3, class = "error.setter.variable.cats.2")
  expect_error(m@var.list$a$cats.eng <- FALSE, class = "error.setter.variable.cats.2")
  expect_error(m@var.list$a$cats.eng <- "", class = "error.setter.variable.cats.3")
  expect_error(m@var.list$a$cats.eng <- c("0==male", "1=female"),
               class = "error.setter.variable.cats.3")
  expect_error(m@var.list$a$cats.eng <- c(male = "0", female = "1"),
               class = "error.setter.variable.cats.3")
  expect_error(m@var.list$a$cats.eng <- c("1.3 = male", "1 = female"),
               class = "error.setter.variable.cats.4")
  expect_error(m@var.list$a$cats.eng <- c("a=male", "1=female"),
               class = "error.setter.variable.cats.5")
  expect_error(m@var.list$a$cats.eng <- c("0=1", "2=female"),
               class = "error.setter.variable.cats.6")
  expect_error(m@var.list$a$cats.eng <- c("-1 = male", "-2 =male"),
               class = "error.setter.variable.cats.7")
  expect_error(m@var.list$a$cats.eng <- c("1 = male", "1 = female"),
               class = "error.setter.variable.cats.8")
  expect_error(m@var.list$a$cats.eng <- c("0 = male", "1 =male"),
               class = "error.setter.variable.cats.9")
})

test_that("setter.select.default works", {
  expect_null(setter.select.default())
  expect_null(setter.select.default(var = NULL, group = NULL, option = NULL,
                                    pre = NULL))
  var <- NULL
  group <- NULL
  option <- NULL
  pre <- NULL
  expect_null(setter.select.default(var = var, group = group, option = option,
                                     pre = pre))
  expect_equal(setter.select.default(var = 1, group = 2, option = 3,
                                     pre = 4), 1)
  expect_equal(setter.select.default(var = NULL, group = 2, option = 3,
                                     pre = 4), 2)
  expect_equal(setter.select.default(var = NULL, group = NULL, option = 3,
                                     pre = 4), 3)
  expect_equal(setter.select.default(var = NULL, group = NULL, option = NULL,
                                     pre = 4), 4)
  expect_true(setter.select.default(var = TRUE, group = NULL, pre = FALSE))
})
