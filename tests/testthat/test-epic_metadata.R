test_that("constructor gives class", {
  expect_s3_class(new_metadata(), "epic_metadata")
  expect_s3_class(empty_metadata(), "epic_metadata")
})

test_that("empty_metadata and validate_metadata fit together", {
  expect_no_error(empty_metadata() %>% validate_metadata())
})

test_that("validate_metadata finds errors", {
  expect_error(new_metadata() %>% validate_metadata())
  # expect_no_error(metadata_party_test %>% validate_metadata())
  #
  # x <- empty_metadata() %>% check_metadata()
  # expect_no_error(validate_metadata(x))
})

# test_that("load_metadata works", {
#   withr::with_tempdir({
#     x <- metadata_party_test
#     saveRDS(x,"metadata.rds")
#     metadata <- load_metadata("metadata.rds")
#     expect_equal(metadata,x)
#   })
# })

test_that("uncheck_metadata works", {
  expect_error(uncheck_metadata(NA), class = "no_epic_metadata")
  expect_error(uncheck_metadata(NULL), class = "no_epic_metadata")
  expect_error(uncheck_metadata(""), class = "no_epic_metadata")

  # x1 <- uncheck_metadata(metadata_party_test)
  # expect_equal(class(x1), "epic_metadata")
  #
  # x2 <- metadata_party_test %>% remove_na() %>% remove_segment("d4") %>% check_metadata()
  # expect_equal(class(x2), "epic_metadata_checked")
  # x2 %<>% uncheck_metadata()
  # expect_equal(class(x2), "epic_metadata")
})
