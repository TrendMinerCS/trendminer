#' Get root structures
#'
#' Gets all root structures including their attributes from TrendMiner and
#' returns it in a data frame.
#'
#' @param token A valid access token
#' @inheritParams tm_get_token
#' @importFrom rlang .data
#' @return A data frame
#' @export
#'
#' @examples
#'   \dontrun{
#'     token <- tm_get_token()
#'     tm_get_root_structures(token)
#'   }
tm_get_root_structures <- function(token, ...) {
  url <- paste(tm_get_base_url(), "/af/assets/browse", sep = "")

  response <- httr::GET(url,
                        httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = "")),
                        httr::user_agent(tm_get_useragent()),
                        httr::accept_json())

  parsed <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()

  total_pages <- parsed$page$totalPages

  content <- vector("list", total_pages)
  content[[1]] <- parsed$content

  if (total_pages == 1) {
    content <- content[[1]] %>%
      select_structure_result_columns()
    return(content)
  }

  # deal with pagination
  for(i in 2:length(content)) {

    next_link <- parsed$links %>%
      dplyr::filter(.data$rel == "next") %>%
      dplyr::select(.data$href) %>%
      sub(".*browse", "", .)

    url <- paste(token$base_url, "/af/assets/browse", next_link, sep = "")

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
  dplyr::bind_rows(content) %>%
    select_structure_result_columns()
}

select_structure_result_columns <- function(df) {
  df %>%
    dplyr::select(-.data$links) %>%
    dplyr::rename(nodeId = .data$nodeIdentifier,
                  structureId = .data$structureIdentifier)
}


