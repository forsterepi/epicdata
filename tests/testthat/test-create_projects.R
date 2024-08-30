test_that("input errors of create_meta work", {
  expect_error(create_meta(name = "test", path = NA_character_), class = "inpur_error_path")
  expect_error(create_meta(name = "test", path = NULL), class = "inpur_error_path")
  expect_error(create_meta(name = "test", path = ""), class = "inpur_error_path")
  expect_error(create_meta(name = NA_character_, path = "test"), class = "inpur_error_name")
  expect_error(create_meta(name = NULL, path = "test"), class = "inpur_error_name")
  expect_error(create_meta(name = "", path = "test"), class = "inpur_error_name")
})

test_that("directory errors of create_meta work", {

})
