#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

tm_get_useragent <- function() {
  ua <- "Request created by the trendminer package; https://github.com/alex23lemm/trendminer"
  ua
}

tm_get_base_url <- function() {
  val <- Sys.getenv("TM_base_url")
  if (identical(val, "")) {
    stop("`TM_base_url` env var has not been set.")
  }
  val
}

tm_get_usr <- function() {
  val <- Sys.getenv("TM_usr")
  if (identical(val, "")) {
    stop("`TM_usr` env var has not been set.")
  }
  val
}

tm_get_pwd <- function() {
  val <- Sys.getenv("TM_pwd")
  if (identical(val, "")) {
    stop("`TM_pwd` env var has not been set.")
  }
  val
}

tm_get_client_ID <- function() {
  val <- Sys.getenv("TM_client_ID")
  if (identical(val, "")) {
    stop("`TM_client_ID` env var has not been set.")
  }
  val
}

tm_get_client_secret <- function() {
  val <- Sys.getenv("TM_client_secret")
  if (identical(val, "")) {
    stop("`TM_client_secret` env var has not been set.")
  }
  val
}

tm_process_paginated_rsp <- function(token, parsed_response, ...) {
  total_pages <- parsed_response$page$totalPages

  if (total_pages == 1) {
    return(parsed_response$content)
  }
  content <- vector("list", total_pages)
  content[[1]] <- parsed_response$content

  # deal with pagination
  for(i in 2:length(content)) {
    next_link <- parsed_response$links %>%
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
    parsed_response <- httr::content(response, as =  "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    content[[i]] <- parsed_response$content
  }
  dplyr::bind_rows(content)
}

