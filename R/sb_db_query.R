#' Query a table
#'
#' @param table The table name (ignored if sql is provided)
#' @param columns Columns to select (default: all)
#' @param where A named list for WHERE clause. Supports operators via nested lists:
#'   `list(id = 1)` for equality, `list(age = list(gt = 25))` for `age > 25`.
#'   Operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is
#' @param limit Maximum rows to return
#' @param sql Raw SQL query (DBI backend only)
#' @param schema The schema name
#' @return A data frame with query results
#' @export
sb_db_query <- function(
  table = NULL,
  columns = "*",
  where = NULL,
  limit = 0,
  sql = NULL,
  schema = get_schema()
) {
  conn <- get_connection()

  if (!is.null(sql)) {
    checkmate::assert_string(sql)
    return(DBI::dbGetQuery(conn, sql))
  }

  checkmate::assert_string(table)

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  select_clause <- if (length(columns) == 1 && columns == "*") {
    DBI::SQL("*")
  } else {
    DBI::SQL(glue::glue_collapse(columns, sep = ", "))
  }

  where_clause <- build_where(where, conn)
  limit_clause <- if (limit > 0) glue::glue_sql("LIMIT {limit}", .con = conn) else DBI::SQL("")

  query <- glue::glue_sql(
    "SELECT {select_clause} FROM {`schema`}.{`table`} {where_clause} {limit_clause}",
    .con = conn
  )

  DBI::dbGetQuery(conn, query)
}
