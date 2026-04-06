#' Read table data via API
#'
#' @param table The table name
#' @param where A list of filter conditions (e.g. list(id = 1
#' or list(created_at = list(gt = "2024-01-01")))
#' @param limit Maximum rows to return (NULL for all)
#' @return A `tibble`
#' @export
sb_api_read <- function(table = NULL, where = NULL, limit = NULL) {
  if (!.sb_env$api_available) {
    cli::cli_abort(
      c(
        "x" = "API credentials not available.",
        "i" = "Set SUPABASE_URL, SUPABASE_ANON_KEY, and SUPABASE_ROLE_KEY."
      ),
      call. = FALSE
    )
  }

  params <- list()
  if (!is.null(where)) {
    params$where <- jsonlite::toJSON(where, auto_unbox = TRUE)
  }
  if (!is.null(limit)) {
    params$limit <- limit
  }

  response <- httr2::request(.sb_env$api_url) |>
    httr2::req_url_path_append("rest/v1") |>
    httr2::req_url_path_append(table) |>
    httr2::req_headers(
      Authorization = paste("Bearer", .sb_env$api_key),
      apikey = Sys.getenv("SUPABASE_ROLE_KEY")
    ) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform()
  if (httr2::resp_status(response) == 200) {
    response |>
      httr2::resp_body_json(simplifyVector = TRUE) |>
      dplyr::as_tibble()
  } else {
    stop(
      "API request failed with status: ",
      httr2::resp_status(response),
      "\nMessage: ",
      httr2::resp_body_string(response),
      call. = FALSE
    )
  }
}
