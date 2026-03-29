#' Insert rows into a table
#'
#' @param table The table name
#' @param data A data frame of rows to insert
#' @param schema The schema name
#' @return Number of rows inserted (invisibly)
#' @example man/examples/sb_db_insert.R
#' @export
sb_db_insert <- function(
  table = NULL,
  data = NULL,
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_data_frame(data),
    combine = "and"
  )

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  n <- DBI::dbAppendTable(
    conn,
    name = DBI::Id(schema = schema, table = table),
    value = data
  )
  cli::cli_alert_success("Inserted {n} row{?s} into {.field {table}}")
  invisible(n)
}
