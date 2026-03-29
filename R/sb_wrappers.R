#' Connect to Supabase
#'
#' Unified connection that auto-detects or uses specified backend.
#'
#' @param backend Backend to use: "auto", "db", or "api"
#' @param schema The schema name (db backend only)
#' @return Invisible connection info
#' @example man/examples/sb_connect.R
#' @export
sb_connect <- function(
  backend = c("auto", "db", "api"),
  schema = Sys.getenv("SUPABASE_SCHEMA", "public")
) {
  backend <- match.arg(backend)

  if (backend == "auto") {
    if (.sb_env$dbi_available) {
      backend <- "db"
    } else if (.sb_env$api_available) {
      backend <- "api"
    } else {
      stop("No credentials found. Set DBI or API environment variables.")
    }
  }

  .sb_env$backend <- backend

  if (backend == "db") {
    sb_db_connect(schema = schema)
  } else if (backend == "api") {
    if (!exists("sb_api_connect", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_connect()
  }
}

#' Disconnect from Supabase
#'
#' @return Invisible NULL
#' @export
sb_disconnect <- function() {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_disconnect()
  } else if (backend == "api") {
    if (exists("sb_api_disconnect", mode = "function")) {
      sb_api_disconnect()
    }
  }

  .sb_env$backend <- NULL
  invisible(NULL)
}

#' Get connection status
#'
#' @return List with connection info
#' @export
sb_status <- function() {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_status()
  } else if (backend == "api") {
    if (!exists("sb_api_status", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_status()
  }
}

#' Read table data
#'
#' @param table The table name
#' @param limit Maximum rows to return (0 for all)
#' @param schema The schema name
#' @return A data frame with table data
#' @export
sb_read <- function(
  table = NULL,
  limit = 0,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_read(
      table = table,
      limit = limit,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_read", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_read(
      table = table,
      limit = limit
    )
  }
}

#' Insert rows into a table
#'
#' @param table The table name
#' @param data A data frame of rows to insert
#' @param schema The schema name
#' @return Number of rows inserted (invisibly)
#' @export
sb_insert <- function(
  table = NULL,
  data = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_insert(
      table = table,
      data = data,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_insert", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_insert(
      table = table,
      data = data
    )
  }
}

#' Update rows in a table
#'
#' @param table The table name
#' @param data A named list of column = value pairs to set
#' @param where A named list for WHERE clause
#' @param schema The schema name
#' @return Number of rows affected (invisibly)
#' @export
sb_update <- function(
  table = NULL,
  data = NULL,
  where = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_update(
      table = table,
      data = data,
      where = where,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_update", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_update(
      table = table,
      data = data,
      where = where
    )
  }
}

#' Delete rows from a table
#'
#' @param table The table name
#' @param where A named list for WHERE clause
#' @param schema The schema name
#' @return Number of rows deleted (invisibly)
#' @export
sb_delete <- function(
  table = NULL,
  where = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_delete(
      table = table,
      where = where,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_delete", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_delete(
      table = table,
      where = where
    )
  }
}

#' Upsert rows into a table
#'
#' @param table The table name
#' @param data A data frame of rows to upsert
#' @param conflict_columns Column(s) to check for conflicts
#' @param schema The schema name
#' @return Number of rows affected (invisibly)
#' @export
sb_upsert <- function(
  table = NULL,
  data = NULL,
  conflict_columns = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_upsert(
      table = table,
      data = data,
      conflict_columns = conflict_columns,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_upsert", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_upsert(
      table = table,
      data = data,
      conflict_columns = conflict_columns
    )
  }
}

#' Query a table
#'
#' @param table The table name
#' @param columns Columns to select
#' @param where A named list for WHERE clause
#' @param limit Maximum rows to return
#' @param sql Raw SQL query (db backend only)
#' @param schema The schema name
#' @return A data frame with query results
#' @export
sb_query <- function(
  table = NULL,
  columns = "*",
  where = NULL,
  limit = 0,
  sql = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_query(
      table = table,
      columns = columns,
      where = where,
      limit = limit,
      sql = sql,
      schema = schema
    )
  } else if (backend == "api") {
    if (!is.null(sql)) {
      stop("Raw SQL not supported with API backend.")
    }
    if (!exists("sb_api_query", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_query(
      table = table,
      columns = columns,
      where = where,
      limit = limit
    )
  }
}

#' List tables
#'
#' @param schema The schema name
#' @return A character vector of table names
#' @export
sb_tables <- function(
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_tables(
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_tables", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_tables()
  }
}

#' Get table schema
#'
#' @param table The table name
#' @param schema The schema name
#' @return A data frame with column info
#' @export
sb_schema <- function(
  table = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_schema(
      table = table,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_schema", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_schema(
      table = table
    )
  }
}

#' Check if table exists
#'
#' @param table The table name
#' @param schema The schema name
#' @return TRUE if table exists
#' @export
sb_table_exists <- function(
  table = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_table_exists(
      table = table,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_table_exists", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_table_exists(
      table = table
    )
  }
}

#' Truncate a table
#'
#' @param table The table name
#' @param schema The schema name
#' @return Invisible NULL
#' @export
sb_truncate <- function(
  table = NULL,
  schema = get_schema()
) {
  backend <- get_backend()

  if (backend == "db") {
    sb_db_truncate(
      table = table,
      schema = schema
    )
  } else if (backend == "api") {
    if (!exists("sb_api_truncate", mode = "function")) {
      stop("API backend not yet implemented.")
    }
    sb_api_truncate(
      table = table
    )
  }
}

#' Get the active backend
#'
#' @return The backend name ("db" or "api")
#' @keywords internal
get_backend <- function() {
  if (is.null(.sb_env$backend)) {
    stop("Not connected. Call sb_connect() first.")
  }
  .sb_env$backend
}
