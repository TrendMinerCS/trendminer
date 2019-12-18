#' General search function on assets and tags
#'
#' Search for assets and tags in the asset hierarchy that match the query pattern.
#'
#' @details
#' `tm_assets_search()` allows to search for nodes in the TrendMiner asset hierachy
#' with arbitary queries. A node either represents an asset (component of the installation)
#' or a tag (property of an asset containing timeseries data). `tm_assets_search()` is
#' powering a couple of other functions under-the-hood like, e.g., `tm_get_assets()` and
#' `tm_get_tags()` which offer a higher abstraction level by using pre-defined search queries.
#'
#' Depending on the query, TrendMiner search results might be paginated.
#' `tm_assets_search()` manages pagination completely on its own by combining all
#' paginated search results in a list before returning them.
#'
#'
#' ## Available query operators
#'
#' * Search operators: equal(==), not equal(!=), in(=in=), not in(=out=)\cr
#' * Logical operator: AND(;), OR(,)\cr
#' * Wildcard operator: *
#'
#' ## List of attributes you can search on
#'
#' id\cr
#' sourceId\cr
#' type\cr
#' externalId\cr
#' template\cr
#' templateId\cr
#' name\cr
#' description\cr
#' dataType\cr
#' data\cr
#' options\cr
#' deleted
#'
#' @param token A valid access token
#' @param query Search query
#' @inheritParams tm_get_token
#' @importFrom rlang .data
#' @return A list with search results. Each list entry represents one page of
#'   a paginated response.
#' @export
#'
#' @examples
#'  \dontrun{
#'    # Retrieve all assets that have "Reactor" in their name
#'    tm_assets_search(token, 'type=="ASSET";name=="*Reactor*"')
#'
#'    # Retrieve all tags that have "Temperature" in their name
#'    tm_assets_search(token, "type=='ATTRIBUTE';name=='*Temperature*'")
#'  }
tm_assets_search <- function(token, query, base_url = NULL, ...) {

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
      dplyr::filter(.data$rel == "next") %>%
      dplyr::select(.data$href) %>%
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

#' Get all tags
#'
#' Gets the complete list of available tags including their attributes from
#' TrendMiner and returns it in a data frame.
#'
#' @inheritParams tm_assets_search
#' @return A data frame with tag information. Each row represents a single tag
#'   and the columns represent specific tag attributes.
#' @export
#'
#' @examples
#'  \dontrun{
#'   tm_get_tags(token)
#'  }
tm_get_tags <- function(token, base_url = NULL, ...) {
  do.call("rbind", tm_assets_search(token, 'type=="ATTRIBUTE"', base_url, ...))
}

