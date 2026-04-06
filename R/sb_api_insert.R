#' Insert rows into a table via API
#'
#' @param table The table name
#' @param data A data frame of rows to insert
#' @param schema The schema name
#' @return Number of rows inserted (invisibly)
#' @example man/examples/sb_api_insert.R
#' @export
sb_api_insert <- function(
  table = NULL,
  data = NULL,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_data_frame(data),
    combine = "and"
  )

  response <- .sb_api_request(
    method = "POST",
    path = paste0("rest/v1/", table),
    body = data,
    prefer = c("count=exact", "return=minimal"),
    schema = schema
  )

  if (!httr2::resp_status(response) %in% c(200L, 201L, 204L)) {
    .sb_api_abort(response)
  }

  n <- .parse_count_header(response)
  cli::cli_alert_success("Inserted {n} row{?s} into {.field {table}}")
  invisible(n)
}
