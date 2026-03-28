#' Update rows in a table
#'
#' @param table The table name
#' @param data A named list of column = value pairs to set
#' @param where A named list for WHERE clause. Supports operators via nested lists.
#' @param schema The schema name
#' @return Number of rows affected (invisibly)
#' @export
sb_db_update <- function(
  table = NULL,
  data = NULL,
  where = NULL,
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_list(data, min.len = 1),
    checkmate::check_list(where, min.len = 1),
    combine = "and"
  )

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  set_clause <- glue::glue_collapse(
    mapply(
      function(col, val) {
        glue::glue_sql("{`col`} = {val}", .con = conn)
      },
      names(data),
      data
    ),
    sep = ", "
  )

  where_clause <- build_where(where, conn, include_keyword = FALSE)

  query <- glue::glue_sql(
    "UPDATE {`schema`}.{`table`} SET {DBI::SQL(set_clause)} WHERE {where_clause}",
    .con = conn
  )

  n <- DBI::dbExecute(conn, query)
  cli::cli_alert_success("Updated {n} row{?s} in {.field {table}}")
  invisible(n)
}
