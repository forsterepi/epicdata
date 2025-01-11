test_that("category values of integer variables are read in correctly", {
  expect_equal(read_meta(file = test_path("fixtures", "cat_load_test.yml"), testing = T)$main$values[1],"1=one or more|2=two|3=three")
  expect_equal(read_meta(file = test_path("fixtures", "cat_load_test.yml"), testing = T)$main$values[2],"1=one or more|2=two|3=three")
  expect_equal(read_meta(file = test_path("fixtures", "cat_load_test.yml"), testing = T)$main$values[3],"1=one or more|2=two|3=three")
  expect_equal(read_meta(file = test_path("fixtures", "cat_load_test.yml"), testing = T)$main$values[4],"1=one or more|2=two|3=three")
})
