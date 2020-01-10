context("Retrieve interpolated time-series data")

token <- tm_token()
start <-  lubridate::ymd_hms("2019-09-15T05:16:00Z")
end <- lubridate::ymd_hms("2019-09-15T05:17:00Z")


test_that("tm_interpolated_data() returns error if token is not of class 'tm_token'", {
  skip_on_cran()

  expect_error(tm_interpoloated_data("not a token", "BA:CONC.1", start, end),
               "'token' must be a TrendMiner access token.")
})

test_that("tm_interplolated_data() returns error if an invalid token is used", {
  skip_on_cran()

  deprecated_token <- token
  deprecated_token$expiration_date <- deprecated_token$expiration_date - 43201
  expect_error(tm_interpoloated_data(deprecated_token, "BA:CONC.1", start, end, 2),
               "Token expired. Please provide a valid access token.")
})

test_that("tm_interplolated_data() returns error if 'tag_name' is not a length one character vector", {
  skip_on_cran()

  expect_error(tm_interpoloated_data(token, 1, start, end, 2),
               "'tag_name' must be a length-one character vector.")
  expect_error(tm_interpoloated_data(token, c("not", "a", "valid", "tag"), start, end, 2),
               "'tag_name' must be a length-one character vector.")
})

test_that("tm_interplolated_data() returns error if 'start_date' is not a POSIXct object'", {
  skip_on_cran()

  expect_error(tm_interpoloated_data(token, "BA:CONC.1",
                                     "2019-09-15T05:16:00Z", end, 2),
               "'start_date' must be a POSIXct object.")
})

test_that("tm_interplolated_data() returns error if 'start_date' time zone is not UTC", {
  skip_on_cran()

  expect_error(tm_interpoloated_data(token, "BA:CONC.1",
                                     lubridate::with_tz(start, "CET"), end, 2),
                                     "'start_date' time zone must be UTC.")
})

test_that("tm_interplolated_data() returns error if 'end_date' is not a POSIXct object'", {
  skip_on_cran()

  expect_error(tm_interpoloated_data(token, "BA:CONC.1",
                                     start, "2019-09-15T05:17:00Z", 2),
               "'end_date' must be a POSIXct object.")
})

test_that("tm_interplolated_data() returns error if 'end_date' time zone is not UTC", {
  skip_on_cran()

  expect_error(tm_interpoloated_data(token, "BA:CONC.1",
                                     start, lubridate::with_tz(end, "CET"), 2),
               "'end_date' time zone must be UTC.")
})

test_that("tm_interplolated_data() returns error if 'start_date' is before 'end_date'", {
  skip_on_cran()

  expect_error(tm_interpoloated_data(token, "BA:CONC.1",
                                     end, start, 2),
               "'start_date' must be before 'end_date'.")
})
