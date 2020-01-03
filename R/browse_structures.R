#' Get root structures
#'
#' Gets all root structures including their attributes and returns them in a
#' data frame.
#'
#' @param token A valid access token
#' @inheritParams tm_token
#' @importFrom rlang .data
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' token <- tm_token()
#'
#' tm_root_structures(token)
#' }
tm_root_structures <- function(token, ...) {
  url <- paste(tm_get_base_url(), "/af/assets/browse", sep = "")

  response <- httr::GET(url,
                        httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = "")),
                        httr::user_agent(tm_get_useragent()),
                        httr::accept_json(),
                        ...)

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


#' Get child structures by parent structure ID
#'
#' Gets the structures that have a parent defined by `parent_id` and returns them
#' in a data frame.
#'
#' @details
#' Depending on the number of child structures, the TrendMiner response might be paginated.
#' `tm_child_structures()` manages pagination completely on its own by combining all
#' paginated child structure results in a data frame before returning them.
#'
#'
#' @param parent_id Parent structure ID
#' @inheritParams tm_root_structures
#' @return A data frame with child structures of `parent_id`.
#' @export
#'
#' @examples
#' \dontrun{
#' token <- tm_token()
#'
#' # Retrieve child structures of the first root structure
#' roots <- tm_root_structures(token)
#' tm_child_structures(token, roots[1, "structureId"])
#'
#' # Get child structures by specific parent structure ID
#' tm_child_structures(token, "e5225244-c6de-48c2-87da-5b51b65062e8")
#' }
tm_child_structures <- function(token, parent_id, ...) {

  if (class(token) != "tm_token") {
    stop("'token' must be a TrendMiner access token.")
  }

  if (class(token) == "tm_token" && !tm_is_valid_token(token)) {
    stop("Token expired. Please provide a valid access token.")
  }

  if (length(parent_id) != 1L || typeof(parent_id) != "character") {
    stop("'parent_id' must be a length-one character vector.")
  }

  url <- paste(token$base_url, "/af/assets/browse?parentId=", parent_id, sep = "")

  response <- httr::GET(url,
                        httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = "")),
                        httr::user_agent(tm_get_useragent()),
                        httr::accept_json(),
                        ...)

  parsed <- httr::content(response, as =  "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  # check if structure represents leaf structure. If yes, return empty list
  if (is.list(parsed$content) & length(parsed$content) == 0)
    return(parsed$content)

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
      unlist(.[1,]) %>%
      unname()

    response <- httr::GET(next_link,
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

#'
#'
tm_descendant_structures <- function(token, parentId) {
  content <- tm_child_structures(token, parentId)

  if (is.list(content) & length(content) == 0)
    return(content)

  for(i in seq_along(content$structureId)) {
    content <- dplyr::bind_rows(content,
                                tm_descendant_structures(token, content$structureId[i]))
  }
 content
}
