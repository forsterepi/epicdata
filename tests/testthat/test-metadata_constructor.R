# test_that("read_meta() shows correct error for empty yaml files", {
#   file <- withr::local_tempfile(pattern = "test", fileext = "yml", lines = "")
#   expect_snapshot(error = TRUE, read_meta(file))
# })

# Change to yaml.read()
