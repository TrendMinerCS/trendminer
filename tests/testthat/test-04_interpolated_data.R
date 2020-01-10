context("Retrieve interpolated time-series data")

token <- tm_token()
start <-  lubridate::ymd_hms("2019-09-15T05:16:00Z")
end <- lubridate::ymd_hms("2019-09-15T05:17:00Z")


test_that("tm_interploated_data() returns the correct result", {
  skip_on_cran()

  rsp <- tm_interpolated_data(token, "BA:CONC.1", start, end, 2)
  expect_identical(names(rsp), c("tag", "timeSeries"))
  expect_identical(names(rsp[[1]]), c("tagName", "shift", "interpolationType"))
  expect_identical(names(rsp[[2]]), c("index", "value"))
  expect_identical(dim(rsp$timeSeries), c(31L, 2L))
})


# Client generated error messages

test_that("tm_interpolated_data() returns error if token is not of class 'tm_token'", {
  skip_on_cran()

  expect_error(tm_interpolated_data("not a token", "BA:CONC.1", start, end),
               "'token' must be a TrendMiner access token.")
})

test_that("tm_interplolated_data() returns error if an invalid token is used", {
  skip_on_cran()

  deprecated_token <- token
  deprecated_token$expiration_date <- deprecated_token$expiration_date - 43201
  expect_error(tm_interpolated_data(deprecated_token, "BA:CONC.1", start, end, 2),
               "Token expired. Please provide a valid access token.")
})

test_that("tm_interplolated_data() returns error if 'tag_name' is not a length one character vector", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, 1, start, end, 2),
               "'tag_name' must be a length-one character vector.")
  expect_error(tm_interpolated_data(token, c("not", "a", "valid", "tag"), start, end, 2),
               "'tag_name' must be a length-one character vector.")
})

test_that("tm_interplolated_data() returns error if 'start_date' is not a POSIXct object'", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, "BA:CONC.1",
                                     "2019-09-15T05:16:00Z", end, 2),
               "'start_date' must be a POSIXct object.")
})

test_that("tm_interplolated_data() returns error if 'start_date' time zone is not UTC", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, "BA:CONC.1",
                                     lubridate::with_tz(start, "CET"), end, 2),
                                     "'start_date' time zone must be UTC.")
})

test_that("tm_interplolated_data() returns error if 'end_date' is not a POSIXct object'", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, "BA:CONC.1",
                                     start, "2019-09-15T05:17:00Z", 2),
               "'end_date' must be a POSIXct object.")
})

test_that("tm_interplolated_data() returns error if 'end_date' time zone is not UTC", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, "BA:CONC.1",
                                     start, lubridate::with_tz(end, "CET"), 2),
               "'end_date' time zone must be UTC.")
})

test_that("tm_interplolated_data() returns error if 'start_date' is before 'end_date'", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, "BA:CONC.1", end, start, 2),
               "'start_date' must be before 'end_date'.")
})

test_that("tm_interplolated_data() returns error if 'step' is is not a length one numeric vector", {
   skip_on_cran()

   expect_error(tm_interpolated_data(token, "BA:CONC.1", start, end, "two"),
                "'step' must be a length-one numeric vector.")
 })

test_that("tm_interplolated_data() returns error if 'step' is smaller than 1", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, "BA:CONC.1", start, end, 0),
               "'step' should be greater or equal than 1.")
})

test_that("tm_interplolated_data() returns error if 'type' is not correct", {
  skip_on_cran()

  expect_error(tm_interpolated_data(token, "BA:CONC.1", start, end, 1, "not_linear"),
               "'type' must be 'linear' or 'stepped'.")
})


# Server generated error messages

test_that("tm_interplolated_data() returns error if 'tag_name' is unknown on the server", {
  skip_on_cran()

  http_400_unknown_tag <- paste("TrendMiner API request failed [400]",
                                "Client error",
                                "Bad Request",
                                "The requested tag is not indexed. Try again after indexing the tag.",
                                sep = "\n")
  expect_error(tm_interpolated_data(token, "BA:CONC.1_0815", start, end, 2),
               http_400_unknown_tag, fixed = TRUE)
})

test_that("tm_interplolated_data() returns error if more than 10.000 data points are requested", {
  skip_on_cran()

  http_400_exceed_10000 <- paste("TrendMiner API request failed [400]",
                                 "Client error",
                                 "Bad Request",
                                 "Invalid request, the maximum number of points that can be requested is 10000.",
                                 sep = "\n")
  expect_error(tm_interpolated_data(token, "BA:CONC.1", start, lubridate::ymd_hms("2019-09-15T08:17:00Z")),
               http_400_exceed_10000, fixed = TRUE)
})

