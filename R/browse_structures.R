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
  if (class(token) != "tm_token") {
    stop("'token' must be a TrendMiner access token.")
  }
  if (class(token) == "tm_token" && !tm_is_valid_token(token)) {
    stop("Token expired. Please provide a valid access token.")
  }
  url <- paste(tm_get_base_url(), "/af/assets/browse", sep = "")
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
  parsed <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()
  content <- tm_process_paginated_rsp(token, parsed, ...)
  content %>%
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
#' @param parent_id Parent structure ID in UUID format
#' @inheritParams tm_root_structures
#' @return A data frame with child structures of `parent_id`.
#' @export
#'
#' @examples
#' \dontrun{
#' token <- tm_token()
#' roots <- tm_root_structures(token)
#'
#' # Get child structures of the first root structure
#' tm_child_structures(token, roots$structureId[1])
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

  # check if structure represents leaf structure. If yes, return empty list
  if (is.list(parsed$content) & length(parsed$content) == 0)
    return(parsed$content)

  content <- tm_process_paginated_rsp(token, parsed, ...)
  content %>%
    select_structure_result_columns()
}

#' Get descendant structures by parent structure ID
#'
#' Retrieves the entire structure subtree underneath `parent_id` and returns it as
#' a data frame.
#'
#' @inheritParams tm_child_structures
#' @return A data frame with all descendant structures of `parent_id`.
#' @export
#'
#' @examples
#' \dontrun{
#' token <- tm_token()
#' roots <- tm_root_structures(token)
#'
#' # Get descendant subtree structure underneath second root structure
#' tm_descendant_structures(token, roots$structureId[2])
#'
#' # Get descendant subtree by specific parent structure id
#' tm_descendant_structures(token, "4e58e3ca-e57d-47b5-8619-20d39626116e")
#' }
tm_descendant_structures <- function(token, parent_id) {
  content <- tm_child_structures(token, parent_id)

  if (is.list(content) & length(content) == 0)
    return(content)

  for(i in seq_along(content$structureId)) {
    content <- dplyr::bind_rows(content,
                                tm_descendant_structures(token, content$structureId[i]))
  }
 content
}
