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

get_useragent <- function() {
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
