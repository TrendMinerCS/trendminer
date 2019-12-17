#' /af/assets/search
#' @export
tm_asset_search <- function(token, query, base_url = NULL, ...) {

  if (class(token) != "tm_token" || !tm_is_valid_token(token)) {
    stop("Please provide a valid access token.")
  }

  if (!is.null(base_url)) {
    if (length(base_url) != 1L || typeof(base_url) != "character") {
      stop("If provided,'base_url' must be a length-one character vector.")
    }
  } else {
    base_url <- tm_get_base_url()
  }

  url <- paste(base_url, "/af/assets/search?query=",
               query, sep = "") %>%
    gsub("\"","%22", .) %>% #encode quotes
    gsub(" ", "%20", .) #encode spaces

  response <- httr::GET(url,
                        httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = "")),
                        httr::user_agent(tm_get_useragent()),
                        httr::accept_json(),
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

  parsed <- httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  total_pages <- parsed$page$totalPages

  content <- vector("list", total_pages)
  content[[1]] <- parsed$content

  if (total_pages == 1)
    return(content)

  # deal with pagination
  for(i in 2:length(content)) {

    next_link <- parsed$links %>%
      dplyr::filter(rel == "next") %>%
      dplyr::select(href) %>%
      sub(".*search", "", .)

    url <- paste(base_url, "/af/assets/search", next_link, sep = "")

    response <- httr::GET(url,
                          httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = "")),
                          httr::user_agent(tm_get_useragent()),
                          httr::accept_json(),
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
    parsed <- httr::content(response, as =  "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    content[[i]] <- parsed$content
  }
  content
}

#' @export
tm_get_tags <- function(token, base_url = NULL, ...) {
  do.call("rbind", tm_asset_search(token, 'type=="ATTRIBUTE"', base_url, ...))
}

