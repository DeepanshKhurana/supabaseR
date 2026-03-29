#' Upsert rows into a table
#'
#' Insert rows, or update if conflict on specified columns.
#'
#' @param table The table name
#' @param data A data frame of rows to upsert
#' @param conflict_columns Column(s) to check for conflicts (e.g., primary key)
#' @param schema The schema name
#' @return Number of rows affected (invisibly)
#' @example man/examples/sb_db_upsert.R
#' @export
sb_db_upsert <- function(
  table = NULL,
  data = NULL,
  conflict_columns = NULL,
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_data_frame(data),
    checkmate::check_character(conflict_columns, min.len = 1),
    combine = "and"
  )

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  cols <- names(data)
  update_cols <- setdiff(cols, conflict_columns)

  col_list <- DBI::SQL(glue::glue_collapse(cols, sep = ", "))
  conflict_list <- DBI::SQL(glue::glue_collapse(conflict_columns, sep = ", "))

  # Build VALUES placeholders

  values_list <- apply(data, 1, function(row) {
    vals <- mapply(function(v) {
      if (is.na(v)) "NULL" else glue::glue_sql("{v}", .con = conn)
    }, row, SIMPLIFY = TRUE)
    glue::glue("({glue::glue_collapse(vals, sep = ', ')})")
  })
  values_sql <- DBI::SQL(glue::glue_collapse(values_list, sep = ", "))

  # Build SET clause for update
  set_clause <- DBI::SQL(glue::glue_collapse(
    sapply(update_cols, function(col) glue::glue("{col} = EXCLUDED.{col}")),
    sep = ", "
  ))

  query <- glue::glue_sql(
    "INSERT INTO {`schema`}.{`table`} ({col_list})
     VALUES {values_sql}
     ON CONFLICT ({conflict_list}) DO UPDATE SET {set_clause}",
    .con = conn
  )

  n <- DBI::dbExecute(conn, query)
  cli::cli_alert_success("Upserted {n} row{?s} into {.field {table}}")
  invisible(n)
}
