#' Get TrendMiner access token
#'
#' Requests a Bearer access token from TrendMiner using an OAuth2.0 resource
#' owner password credentials grant flow.
#'
#' All requests to the TrendMiner API require authentication. This is achieved
#' by sending a valid Bearer access token in the request headers. Request tokens
#' are obtained via OAuth2.0 using the resource owner password credentials flow:
#' Any client which likes to interact with the API needs to
#' collect the credentials from the user (username and password) and passes them
#' together with its own client credentials (client ID and client secret)
#' to the TrendMiner server. The server responds with an access token which the
#' user needs to use for any subsequent API requests.
#'
#' @details
#' User credentials, client credentials and the TrendMiner base URL can be passed
#' as arguments to `tm_get_token()` for quick testing in interactive mode.
#' However, it is recommended to call `tm_get_token()` without arguments.
#' In this case `tm_get_token()` will fetch the credentials and the TrendMiner
#' base URL from the following environment variables stored in
#' `.Renviron`:
#'
#' `TM_client_ID = YOUR_CLIENT_ID_HERE`\cr
#' `TM_secret = YOUR_CLIENT_SECRET_HERE`\cr
#' `TM_usr = YOUR_USER_NAME_HERE`\cr
#' `TM_pwd = YOUR_USER_PASSWORD_HERE`\cr
#' `TM_base_url = YOUR_TM_BASE_URL_HERE`
#' @param client_id Client identifier issued by the authorization server
#' @param client_secret Client secret issued by the authorization server
#' @param usr_name Username
#' @param usr_pwd User password
#' @param base_url TrendMiner base URL
#' @param ... Additional arguments passed on to the underlying HTTP method.
#'   This might be necessary if you need to set some curl options explicitly
#'   via \code{\link[httr]{config}}.
#' @return A S3 object of class `tm_token` (basically a list) with the following
#'  components:
#' * `access_token` The access token which needs to be used for any subsequent API request
#' * `token_type` Type of the token which is always "bearer"
#' * `expires_in` Token expiration time in seconds
#' * `scope` Character string describing the access scope
#' * `allowedHistorians` Character string describing the Historians
#'   which can be accessed with the `access_token`
#' * `userId` The user's ID which will be used for any action performed
#'   on the connected TrendMiner instance using the `access_token`
#' * `expiration_date` "POSIXct" object representing the date the token will expire
#' * `base_url` TrendMiner base URL
#' @export
#'
#' @examples
#' \dontrun{
#' tm_get_token()
#' }
tm_get_token <- function(client_id = NULL, client_secret = NULL,
                         usr_name = NULL , usr_pwd = NULL, base_url = NULL, ...) {

  if (!is.null(client_id)) {
    if (length(client_id) != 1L || typeof(client_id) != "character") {
      stop("If provided,'client_id' must be a length-one character vector.")
    }
  } else {
    client_id <- tm_get_client_ID()
  }

  if (!is.null(client_secret)) {
    if (length(client_secret) != 1L || typeof(client_secret) != "character") {
      stop("If provided,'client_secret' must be a length-one character vector.")
    }
  } else {
    client_secret <- tm_get_client_secret()
  }

  if (!is.null(usr_name)) {
    if (length(usr_name) != 1L || typeof(usr_name) != "character") {
      stop("If provided,'usr_name' must be a length-one character vector.")
    }
  } else {
    usr_name <- tm_get_usr()
  }

  if (!is.null(usr_pwd)) {
    if (length(usr_pwd) != 1L || typeof(usr_pwd) != "character") {
      stop("If provided,'usr_pwd' must be a length-one character vector.")
    }
  } else {
    usr_pwd <- tm_get_pwd()
  }

  if (!is.null(base_url)) {
    if (length(base_url) != 1L || typeof(base_url) != "character") {
      stop("If provided,'base_url' must be a length-one character vector.")
    }
  } else {
    base_url <- tm_get_base_url()
  }

  url <- paste(base_url, "security/oauth/token", sep = "/")
  body = list(scope = "read",
              grant_type = "password",
              username = usr_name,
              password = usr_pwd)

  response <- httr::POST(url, httr::authenticate(client_id,
                                                 client_secret),
                         httr::user_agent(get_useragent()),
                         httr::accept_json(),
                         encode = "form",
                         body = body,
                         ...)

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "TrendMiner API request failed [%s]\n%s\n%s\n%s",
        httr::status_code(response),
        httr::http_status(response)$category,
        httr::http_status(response)$reason,
        httr::http_status(response)$message
      ),
      call. = FALSE
    )
  }

  curr_time <- Sys.time()
  parsed <- httr::content(response, as =  "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  parsed[["expiration_date"]] <- curr_time + parsed[["expires_in"]]

  structure(
    parsed,
    class = "tm_token"
  )
}

#' @export
print.tm_token <- function(x, ...) {
  utils::str(x)
  invisible(x)
}

tm_validate_token <- function(token) {
  token[["expiration_date"]] > Sys.time()
}

