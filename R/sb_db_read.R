#' Read table data
#'
#' @param table The table name
#' @param limit Maximum rows to return (0 for all)
#' @param schema The schema name
#' @return A data frame with table data
#' @example man/examples/sb_db_read.R
#' @export
sb_db_read <- function(
  table = NULL,
  limit = 0,
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_numeric(limit),
    combine = "and"
  )

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  limit_clause <- if (limit > 0) glue::glue_sql("LIMIT {limit}", .con = conn) else DBI::SQL("")

  DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "SELECT * FROM {`schema`}.{`table`} {limit_clause}",
      .con = conn
    )
  )
}
