#' Check API backend status
#'
#' @return A list with API connection info
#' @example man/examples/sb_api_status.R
#' @export
sb_api_status <- function() {
  list(
    connected = isTRUE(.sb_env$api_available),
    url = .sb_env$api_url,
    schema = get_schema()
  )
}
