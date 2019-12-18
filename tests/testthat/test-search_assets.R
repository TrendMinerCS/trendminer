context("asset search")

test_that("tm_get_tags() returns a data frame with all tags", {
  skip_on_cran()
  token <- tm_get_token()
  tags <- tm_get_tags(token)
  expect_identical(dim(tags), c(59L, 17L))
})

test_that("tm_search_assets() returns expected results", {
  skip_on_cran()

})
