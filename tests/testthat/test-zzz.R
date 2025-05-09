test_that("%!in% works", {
  expect_equal(NULL %!in% NULL, NULL %in% NULL)
  expect_equal(NULL %!in% "a", NULL %in% "a")
  expect_true("a" %!in% NULL)
  expect_false(NA %!in% NA)
  expect_true("a" %!in% NA)
  expect_true(NA %!in% "a")
  expect_true("a" %!in% 3)
  expect_true(3 %!in% "a")
  expect_true("a" %!in% data.frame(b = c("c","d"), c = c(1,2)))
  expect_true("a" %!in% data.frame(a = c("c","d"), c = c(1,2)))
  expect_true("a" %!in% data.frame(b = c("a","d"), c = c(1,2)))
  expect_false("a" %!in% data.frame(b = c("a","d"), c = c(1,2))$b)
  expect_equal(data.frame(b = c("c","d"), c = c(1,2)) %!in% "a", c(TRUE, TRUE))
  expect_false("a" %!in% c("a","b"))
  expect_false("a" %!in% "a")
  expect_true("a" %!in% c("b","c"))
  expect_true("a" %!in% "b")
})
