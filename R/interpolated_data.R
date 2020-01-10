#' Tags
#'
#' @param token A valid access token
#' @param tag_name Tag name
#' @param start_date POSIXct object
#' @param end_date POSIXct object
#' @param step Time between returned time series data points expressed in seconds
#' @param type Interpolation type which is either "linear" or "stepped"
#' @param shift Time series offset expressed in seconds
#' @inheritParams tm_token
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
tm_interpoloated_data <- function(token, tag_name, start_date, end_date,
                                  step = 1,
                                  type = "linear",
                                  shift = 0) {
  if (class(token) != "tm_token") {
    stop("'token' must be a TrendMiner access token.")
  }
  if (class(token) == "tm_token" && !tm_is_valid_token(token)) {
    stop("Token expired. Please provide a valid access token.")
  }
  if (length(tag_name) != 1L || typeof(tag_name) != "character") {
    stop("'tag_name' must be a length-one character vector.")
  }
  if (class(start_date)[1] != "POSIXct") {
    stop("'start_date' must be a POSIXct object.")
  }
  if (attr(start_date, "tzone") != "UTC") {
    stop("'start_date' time zone must be UTC.")
  }
  if (class(end_date)[1] != "POSIXct") {
    stop("'end_date' must be a POSIXct object.")
  }
  if (attr(end_date, "tzone") != "UTC") {
    stop("'end_date' time zone must be UTC.")
  }
  if (start_date > end_date) {
    stop("'start_date' must be before 'end_date'.")
  }

  url <- paste(tm_get_base_url(), "/compute/interpolatedData", sep = "")
  create_body <- function(step, tag_name, type,
                          shift, start_date, end_date) {
    body <- list()
    body$stepInSeconds <- step
    tag <- list()
    tag$id <- tag_name
    tag$interpolationType <- type
    tag$shift <- shift

    timePeriod <- list()
    timePeriod$startDate <- format(start_date, "%Y-%m-%dT%H:%M:%OSZ")
    timePeriod$endDate <- format(end_date, "%Y-%m-%dT%H:%M:%OSZ")

    body$tag <- tag
    body$timePeriod <- timePeriod
    body
  }

  body <- create_body(step, tag_name, type, shift, start_date, end_date)
  url <- paste(tm_get_base_url(), "/compute/interpolatedData", sep = "")

  response <- httr::POST(url,
                         httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = "")),
                         httr::user_agent(tm_get_useragent()),
                         httr::accept_json(),
                         body = body,
                         encode = "json")

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "TrendMiner API request failed [%s]\n%s\n%s\n%s",
        httr::status_code(response),
        httr::http_status(response)$category,
        httr::http_status(response)$reason,
        httr::content(response)$message
      ),
    call. = FALSE
    )
  }
  parsed <- httr::content(response, as =  "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  parsed
}
