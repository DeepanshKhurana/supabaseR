#' Insert or update a table row
#'
#' @param table_name The name of the table.
#' @param input_list A list of column-value pairs.
#' @param is_update Whether the operation is an update.
#' @param schema The schema name.
#' @param conn A database connection object.
#' @export
put_table_row <- function(
  table_name = NULL,
  input_list = list(),
  is_update = FALSE,
  schema = "public",
  conn = make_connection()
) {
  on.exit(DBI::dbDisconnect(conn))
  checkmate::assert(
    checkmate::check_string(table_name),
    checkmate::check_list(input_list),
    checkmate::check_logical(is_update),
    combine = "and"
  )

  if (is_valid_table(table_name, schema, conn)) {
    table_schema <- get_table_schema(table_name, schema)
    columns <- filter_columns(table_schema, is_update)

    if (!is_update) {
      input_list <- c(
        id = 1 + get_latest_key(
          table_name,
          schema = schema,
          conn = conn
        ),
        input_list
      )
    }

    names(input_list) <- columns

    input_list <- lapply(
      seq_along(input_list),
      function(index) {
        tryCatch(
          expr = {
            FUN <- map_sql_to_r( # nolint
              table_schema$data_type[index] |>
                unlist()
            ) |>
              get()
            if (identical(FUN, as.Date)) { # nolint
              FUN( # nolint
                input_list[index] |>
                  as.character(),
                format = "%Y-%m-%d"
              )
            } else {
              FUN(
                input_list[index] |>
                  as.character()
              )
            }
          }
        )
      }
    )

    values <- lapply(
      input_list,
      function(value) {
        if (inherits(value, "Date")) {
          DBI::dbQuoteLiteral(conn, value)
        } else if (is.character(value)) {
          if (gsub('"|\'', "", value) == "NA") {
            DBI::dbQuoteLiteral(conn, "")
          } else {
            DBI::dbQuoteLiteral(conn, value)
          }
        } else if (is.numeric(as.numeric(value))) {
          as.numeric(value)
        } else {
          print("Error occurred while parsing: {value}")
          stop("Check put_table_row()!")
        }
      }
    )

    if (is_update) {
      set_clause <- glue::glue_sql_collapse(
        mapply(
          function(col, val) {
            glue::glue_sql(
              "{`col`} = {val}",
              .con = conn
            )
          },
          columns,
          values,
          SIMPLIFY = FALSE
        ),
        sep = ", "
      )
      query <- glue::glue_sql(
        "UPDATE {`schema`}.{`table_name`}
         SET {set_clause}
         WHERE id = {input_list[[1]]}",
        .con = conn
      )
    } else {
      query <- glue::glue_sql(
        "INSERT INTO {`schema`}.{`table_name`}
         ({glue::glue_sql_collapse(`columns`, sep = ', ')})
         VALUES ({glue::glue_sql_collapse(values, sep = ', ')})",
        .con = conn
      )
    }

    DBI::dbExecute(conn, query)
  } else {
    stop(glue::glue("Table '{table_name}' does not exist!"))
  }
}
