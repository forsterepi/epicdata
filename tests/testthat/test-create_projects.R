test_that("input errors of create_meta work", {
  expect_error(create_meta(name = "test", path = NA_character_), class = "inpur_error_path")
  expect_error(create_meta(name = "test", path = NULL), class = "inpur_error_path")
  expect_error(create_meta(name = "test", path = ""), class = "inpur_error_path")
  expect_error(create_meta(name = NA_character_, path = "test"), class = "inpur_error_name")
  expect_error(create_meta(name = NULL, path = "test"), class = "inpur_error_name")
  expect_error(create_meta(name = "", path = "test"), class = "inpur_error_name")
  expect_error(create_meta(name = "test", path = "test", db = "test"), class = "inpur_error_db")
})

test_that("directory errors of create_meta work", {

})

test_that("input errors of create_raw work", {
  expect_error(create_raw(name = "test", path = NA_character_), class = "inpur_error_path")
  expect_error(create_raw(name = "test", path = NULL), class = "inpur_error_path")
  expect_error(create_raw(name = "test", path = ""), class = "inpur_error_path")
  expect_error(create_raw(name = NA_character_, path = "test"), class = "inpur_error_name")
  expect_error(create_raw(name = NULL, path = "test"), class = "inpur_error_name")
  expect_error(create_raw(name = "", path = "test"), class = "inpur_error_name")
  expect_error(create_raw(name = "test", path = "test", db = "test"), class = "inpur_error_db")
})

test_that("directory errors of create_raw work", {

})

test_that("input errors of create_prc work", {
  expect_error(create_prc(name = "test", path = NA_character_), class = "inpur_error_path")
  expect_error(create_prc(name = "test", path = NULL), class = "inpur_error_path")
  expect_error(create_prc(name = "test", path = ""), class = "inpur_error_path")
  expect_error(create_prc(name = NA_character_, path = "test"), class = "inpur_error_name")
  expect_error(create_prc(name = NULL, path = "test"), class = "inpur_error_name")
  expect_error(create_prc(name = "", path = "test"), class = "inpur_error_name")
  expect_error(create_prc(name = "test", path = "test", db = "test"), class = "inpur_error_db")
})

test_that("directory errors of create_prc work", {

})
