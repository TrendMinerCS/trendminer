context("browse structures")

token <- tm_token()

test_that("tm_root_structures() returns the expected results", {
  skip_on_cran()

  response <- tm_root_structures(token)
  expect_identical(dim(response), c(3L, 12L))
  response <- response[order(response$name), ]
  rownames(response) <- NULL
  expect_identical(response[, c("structureId", "nodeId")],
                   data.frame(structureId = c("2b66622f-83c7-48e9-9ccc-6dd9214e70c6",
                                              "783249ff-bfc3-4453-bd6a-27d9e71f03e2",
                                              "ca12dc39-516d-4217-b7cc-a8d220a32858"),
                              nodeId = c("20d6b09f-d252-4e27-9dec-c4b89ef2c949",
                                         "15529c52-ae81-4a23-a988-91e6551cb1a7",
                                         "876381a3-3dac-42f8-b72b-eac9d7be42b8"),
                              stringsAsFactors = FALSE))
})

test_that("tm_child_structures() returns the expected results", {
  skip_on_cran()

  response <- tm_child_structures(token, "2b66622f-83c7-48e9-9ccc-6dd9214e70c6")
  expect_identical(dim(response), c(2L, 14L))
  expect_identical(response[["structureId"]], c("1aef0aa1-f942-441e-82d8-0c4bfe7208b3",
                                              "4e58e3ca-e57d-47b5-8619-20d39626116e"))
  response <- tm_child_structures(token, "f8ae4783-c265-4d88-8e4c-59d7942a9385")
  expect_identical(dim(response), c(9L, 16L))
})


test_that("tm_child_structures() returns error if token is not of class 'tm_token'", {
  skip_on_cran()

  expect_error(tm_child_structures("not_a_token", "4e58e3ca-e57d-47b5-8619-20d39626116e"),
               "'token' must be a TrendMiner access token.")
})

test_that("tm_child_structures() returns error if an invalid token is used", {
  skip_on_cran()

  deprecated_token <- token
  deprecated_token$expiration_date <- deprecated_token$expiration_date - 43201
  expect_error(tm_child_structures(deprecated_token, "4e58e3ca-e57d-47b5-8619-20d39626116e"),
               "Token expired. Please provide a valid access token.")
})

test_that("tm_child_structures() returns error if 'parent_id' is not length one character vector", {
  skip_on_cran()

  expect_error(tm_child_structures(token, 1), "'parent_id' must be a length-one character vector.")
  expect_error(tm_child_structures(token, c("not", "a", "single", "parent_id")),
               "'parent_id' must be a length-one character vector")
})

test_that("tm_descendant_structures() returns the expected results", {
  skip_on_cran()

  barca_site <- tm_descendant_structures(token, "2b66622f-83c7-48e9-9ccc-6dd9214e70c6")
  expect_identical(dim(barca_site), c(29L, 16L))
  catalyst_subtree <- tm_descendant_structures(token, "4e58e3ca-e57d-47b5-8619-20d39626116e")
  expect_identical(dim(catalyst_subtree), c(17L, 16L))
})

