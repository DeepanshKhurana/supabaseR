#' Delete rows from a table
#'
#' @param table The table name
#' @param where A named list for WHERE clause. Supports operators via nested lists.
#' @param schema The schema name
#' @return Number of rows deleted (invisibly)
#' @example man/examples/sb_db_delete.R
#' @export
sb_db_delete <- function(
  table = NULL,
  where = NULL,
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_list(where, min.len = 1),
    combine = "and"
  )

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  where_clause <- build_where(where, conn, include_keyword = FALSE)

  query <- glue::glue_sql(
    "DELETE FROM {`schema`}.{`table`} WHERE {where_clause}",
    .con = conn
  )

  n <- DBI::dbExecute(conn, query)
  cli::cli_alert_success("Deleted {n} row{?s} from {.field {table}}")
  invisible(n)
}
