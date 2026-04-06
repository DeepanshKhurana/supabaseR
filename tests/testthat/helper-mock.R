#' Create a minimal httr2_response mock
#'
#' Returns a structure that satisfies httr2::resp_status(),
#' httr2::resp_body_json(), httr2::resp_header(), and httr2::resp_body_string().
#' Headers are lowercase to match httr2 internals.
#'
#' @param status_code HTTP status code (default 200)
#' @param body JSON string body (default "[]")
#' @param content_range Optional Content-Range header value, e.g. "*/42"
make_mock_response <- function(
  status_code = 200L,
  body = "[]",
  content_range = NULL
) {
  hdrs <- list(`content-type` = "application/json")
  if (!is.null(content_range)) {
    hdrs[["content-range"]] <- content_range
  }
  structure(
    list(
      method = "GET",
      url = "https://test.supabase.co/rest/v1/test",
      status_code = as.integer(status_code),
      headers = structure(hdrs, class = "httr2_headers"),
      body = charToRaw(body),
      request = structure(
        list(url = "https://test.supabase.co/rest/v1/test",
             method = NULL, headers = list(), body = NULL,
             fields = list(), options = list(), policies = list(),
             state = new.env(parent = emptyenv())),
        class = "httr2_request"
      ),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}
