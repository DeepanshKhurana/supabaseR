#' Build WHERE clause from list
#'
#' @param where A named list of conditions
#' @param conn Database connection for escaping
#' @param include_keyword Include "WHERE" keyword (default TRUE)
#' @return SQL string for WHERE clause
#' @details
#' Supports operators via nested lists:
#' - `list(id = 1)` → `id = 1`
#' - `list(age = list(gt = 25))` → `age > 25`
#' - `list(name = list(like = "A%"))` → `name LIKE 'A%'`
#'
#' Supported operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is
build_where <- function(
  where,
  conn,
  include_keyword = TRUE
) {
  if (is.null(where) || length(where) == 0) {
    return(DBI::SQL(""))
  }

  operators <- list(
    eq = "=",
    neq = "<>",
    gt = ">",
    gte = ">=",
    lt = "<",
    lte = "<=",
    like = "LIKE",
    ilike = "ILIKE",
    "in" = "IN",
    is = "IS"
  )

  conditions <- mapply(
    function(col, val) {
      if (is.list(val) && length(val) == 1) {
        op_name <- names(val)[1]
        op_val <- val[[1]]
        op_sql <- operators[[op_name]]

        if (is.null(op_sql)) {
          stop(glue::glue("Unknown operator: {op_name}"))
        }

        if (op_name == "in") {
          vals <- glue::glue_sql("{op_val*}", .con = conn)
          glue::glue_sql("{`col`} IN ({vals})", .con = conn)
        } else if (op_name == "is") {
          glue::glue_sql("{`col`} IS {DBI::SQL(op_val)}", .con = conn)
        } else {
          glue::glue_sql("{`col`} {DBI::SQL(op_sql)} {op_val}", .con = conn)
        }
      } else {
        glue::glue_sql("{`col`} = {val}", .con = conn)
      }
    },
    names(where),
    where,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  conditions_sql <- DBI::SQL(glue::glue_collapse(conditions, sep = " AND "))
  if (include_keyword) {
    glue::glue_sql("WHERE {conditions_sql}", .con = conn)
  } else {
    conditions_sql
  }
}
