#' Truncate a table via API
#'
#' Deletes all rows from the table using a DELETE request with no filter.
#' Requires the secret key or RLS must be disabled for the table.
#'
#' @param table The table name
#' @param schema The schema name
#' @return Invisible NULL
#' @example man/examples/sb_api_truncate.R
#' @export
sb_api_truncate <- function(
  table = NULL,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert_string(table)

  response <- .sb_api_request(
    method = "DELETE",
    path = paste0("rest/v1/", table),
    prefer = c("return=minimal"),
    schema = schema
  )

  if (!httr2::resp_status(response) %in% c(200L, 204L)) {
    .sb_api_abort(response)
  }

  cli::cli_alert_success("Truncated {.field {table}}")
  invisible(NULL)
}
