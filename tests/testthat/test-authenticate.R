context("authentication")

test_that("tm_get_token() returns Bearer access token when fetching
          all credentials from .Renviron", {
  skip_on_cran()

  token <- tm_get_token()
  expect_length(token, 7)
  expect_equal(names(token), c("access_token", "token_type", "expires_in",
                               "scope", "allowedHistorians", "userId",
                               "expiration_date"))

})

test_that("tm_get_token() returns Bearer access token when the user passes the
          credentials manually", {
  skip_on_cran()

  token <- tm_get_token(client_id = tm_get_client_ID(),
                        client_secret = tm_get_client_secret(),
                        usr_name = tm_get_usr(),
                        usr_pwd = tm_get_pwd())
  expect_length(token, 7)
  expect_equal(names(token), c("access_token", "token_type", "expires_in",
                               "scope", "allowedHistorians", "userId",
                               "expiration_date"))
})

test_that("tm_get_token() throws and error when trying to authenticate with
          the wrong credentials", {
  skip_on_cran()

  error_msg_400 <- paste("TrendMiner API request failed [400]",
                         "Client error",
                         "Bad Request",
                         "Client error: (400) Bad Request",
                          sep = "\n")
  error_msg_401 <- paste("TrendMiner API request failed [401]",
                         "Client error",
                         "Unauthorized",
                         "Client error: (401) Unauthorized",
                         sep = "\n")

  expect_error(tm_get_token(client_id = "unknown_id"), error_msg_401, fixed = TRUE)
  expect_error(tm_get_token(client_secret = "unkown_secret"), error_msg_401, fixed = TRUE)
  expect_error(tm_get_token(usr_name = "unknown_user"), error_msg_400, fixed = TRUE)
  expect_error(tm_get_token(usr_pwd = "unknown_pwd"), error_msg_400, fixed = TRUE)
})

test_that("tm_get_token() throws error when incorrect objects are passed as arguments", {
  skip_on_cran()

  expect_error(tm_get_token(client_id = 42),
    "If provided,'client_id' must be a length-one character vector.")
  expect_error(tm_get_token(client_secret = 42),
    "If provided,'client_secret' must be a length-one character vector.")
  expect_error(tm_get_token(usr_name = c("a", "b")),
    "If provided,'usr_name' must be a length-one character vector.")
  expect_error(tm_get_token(usr_pwd = c("a", "b")),
    "If provided,'usr_pwd' must be a length-one character vector.")
})

