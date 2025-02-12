test_that("constructor gives class", {
  expect_s3_class(new_meta(), "epicdata_meta")
})

test_that("validate_metadata finds errors", {
  expect_error(new_meta() %>% validate_meta())
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
