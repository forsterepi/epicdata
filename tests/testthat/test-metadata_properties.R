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
