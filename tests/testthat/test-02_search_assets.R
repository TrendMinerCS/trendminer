context("asset search")

token <- tm_get_token()

test_that("tm_search_assets() returns expected results", {
  skip_on_cran()

  response <- tm_search_assets(token, 'name== "Reactor Level",data=="BA2:CONC.1"')
  parsed <- response[, c("name", "data")]
  expect_identical(dim(response), c(3L, 11L))
  expect_identical(parsed, data.frame(name = c("Reactor Concentration", "Reactor Level", "Reactor Level"),
                                      data = c("BA2:CONC.1", "BA2:LEVEL.1", "BA:LEVEL.1"),
                                      stringsAsFactors = FALSE))
  expect_identical(dim(tm_search_assets(token, 'type=="ASSET";name=="*Reactor*"')),
                   c(6L, 9L))
})

test_that("tm_search_assets() returns error if token is not of class 'tm_token'", {
  skip_on_cran()

  expect_error(tm_search_assets("not_a_token"), "'token' must be a TrendMiner access token.")
})

test_that("tm_search_assets() returns error if an invalid token is used", {
  skip_on_cran()

  deprecated_token <- token
  deprecated_token$expiration_date <- deprecated_token$expiration_date - 43201
  expect_error(tm_search_assets(deprecated_token, "Token expired. Please provide a valid access token."))
})

test_that("tm_search_assets() returns error if 'query' is not length one character vector", {
  skip_on_cran()

  expect_error(tm_search_assets(token, 1), "'query' must be a length-one character vector.")
  expect_error(tm_search_assets(token, c("not", "a", "query")), "'query' must be a length-one character vector")
})

test_that("tm_get_tags() returns a data frame with all tags", {
  skip_on_cran()

  tags <- tm_get_tags(token)
  expect_identical(dim(tags), c(59L, 11L))
})


