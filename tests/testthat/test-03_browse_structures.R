context("browse structures")

token <- tm_get_token()

test_that("tm_get_root_structures returns expected results", {
  skip_on_cran()

  response <- tm_get_root_structures(token)
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
