#' Search on assets and tags
#'
#' Search for assets and tags in the asset framework that match the query pattern.
#'
#' @details
#' `tm_search_assets()` allows to search for nodes in the TrendMiner asset framework
#' with arbitrary queries. A node either represents an asset (component of the plant)
#' or a tag (attribute of an asset storing timeseries data). `tm_search_assets()` is
#' powering a couple of other functions under-the-hood like, e.g., `tm_assets()` and
#' `tm_tags()` which offer a higher abstraction level by using pre-defined search queries.
#'
#' ## Available query operators
#'
#' * Search operators: equal(==), not equal(!=), in(=in=), not in(=out=)\cr
#' * Logical operator: AND(;), OR(,)\cr
#' * Wildcard operator: *
#'
#' ## List of node properties you can search on
#'
#' * `id` Node id (in UUID format)\cr
#' * `sourceId`\cr
#' * `type` Type of the node. Either "ASSET" or "ATTRIBUTE" whereas the later refers to a tag\cr
#' * `externalId`\cr
#' * `template`\cr
#' * `templateId`\cr
#' * `name` Name of the node\cr
#' * `description` Description of the node\cr
#' * `options`\cr
#' * `deleted` Boolean value. Either "TRUE" or "FALSE"
#' * `dataType` Only available for nodes of type "ATTRIBUTE"\cr
#' * `data` Tag name. Only available for nodes of type "ATTRIBUTE"\cr
#'
#' @param token A valid access token
#' @param query Search query
#' @inheritParams tm_token
#' @importFrom rlang .data
#' @return A data frame with search results. Each row represents one asset/tag which
#'   matched the query pattern. The column names of the data frame returned
#'   correspond to the search properties listed in **Details**. The only exception
#'   from this pattern are the `ìd` search property which will be represented
#'   by the `nodeId` column and the `data` search property which will be represented
#'   by the `tagName` column. Data frames containing only asset but no tag
#'   search results won't include the `dataType` and `tagName` columns.
#' @export
#'
#' @examples
#' \dontrun{
#' token <- tm_token()
#'
#' # Retrieve all assets that have "Reactor" in their name
#' tm_search_assets(token, 'type=="ASSET";name=="*Reactor*"')
#'
#' # Retrieve all tags whose name starts with "Temperature"
#' tm_search_assets(token, "type=='ATTRIBUTE';name=='Temperature*'")
#' }
tm_search_assets <- function(token, query, ...) {
  if (class(token) != "tm_token") {
    stop("'token' must be a TrendMiner access token.")
  }
  if (class(token) == "tm_token" && !tm_is_valid_token(token)) {
    stop("Token expired. Please provide a valid access token.")
  }
  if (length(query) != 1L || typeof(query) != "character") {
    stop("'query' must be a length-one character vector.")
  }
  url <- paste(token$base_url, "/af/assets/search?query=",
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
  content <- tm_process_paginated_rsp(token, parsed, ...)
  content %>%
    select_node_result_columns()
}


select_node_result_columns <- function(df) {
  df %>%
    dplyr::select(.data$nodeIdentifier, .data$sourceId, .data$type, .data$externalId,
                  .data$template, .data$templateId, .data$name, .data$description,
                  .data$deleted, dplyr::contains("data")) %>%
    {if("data" %in% names(.)) dplyr::rename(., tagName = .data$data) else .} %>%
    dplyr::rename(nodeId = .data$nodeIdentifier)
}


#' Get all tags
#'
#' Gets the complete list of available tags and returns it as a data frame.
#'
#' @inheritParams tm_search_assets
#' @return A data frame with tag information. Each row represents a single tag
#'   and the columns represent specific tag properties.
#' @export
#'
#' @examples
#' \dontrun{
#' token <- tm_token()
#'
#' tm_tags(token)
#' }
tm_tags <- function(token, ...) {
  tm_search_assets(token, 'type=="ATTRIBUTE"', ...)
}


#' Get all assets
#'
#' Gets the complete list of available assets and returns it as a data frame.
#'
#' @inheritParams tm_search_assets
#' @return A data frame with asset information. Each row represents a single asset
#'   and the columns represent specific asset properties.
#' @export
#'
#' @examples
#' \dontrun{
#' token <- tm_token()
#'
#' tm_assets(token)
#' }
tm_assets <- function(token, ...) {
  tm_search_assets(token, 'type=="ASSET"', ...)
}
