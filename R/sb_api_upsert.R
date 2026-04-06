#' Upsert rows into a table via API
#'
#' Insert rows, or update on conflict with specified columns.
#'
#' @param table The table name
#' @param data A data frame of rows to upsert
#' @param conflict_columns Column(s) to check for conflicts (e.g., primary key)
#' @param schema The schema name
#' @return Number of rows affected (invisibly)
#' @example man/examples/sb_api_upsert.R
#' @export
sb_api_upsert <- function(
  table = NULL,
  data = NULL,
  conflict_columns = NULL,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_data_frame(data),
    checkmate::check_character(conflict_columns, min.len = 1),
    combine = "and"
  )

  params <- list(on_conflict = paste(conflict_columns, collapse = ","))

  response <- .sb_api_request(
    method = "POST",
    path = paste0("rest/v1/", table),
    params = params,
    body = data,
    prefer = c("resolution=merge-duplicates", "count=exact", "return=minimal"),
    schema = schema
  )

  if (!httr2::resp_status(response) %in% c(200L, 201L, 204L)) {
    .sb_api_abort(response)
  }

  n <- .parse_count_header(response)
  cli::cli_alert_success("Upserted {n} row{?s} into {.field {table}}")
  invisible(n)
}
