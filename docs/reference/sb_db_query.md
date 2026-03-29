# Query a table

Query a table

## Usage

``` r
sb_db_query(
  table = NULL,
  columns = "*",
  where = NULL,
  limit = 0,
  sql = NULL,
  schema = get_schema()
)
```

## Arguments

- table:

  The table name (ignored if sql is provided)

- columns:

  Columns to select (default: all)

- where:

  A named list for WHERE clause. Supports operators via nested lists:
  `list(id = 1)` for equality, `list(age = list(gt = 25))` for
  `age > 25`. Operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is

- limit:

  Maximum rows to return

- sql:

  Raw SQL query (DBI backend only)

- schema:

  The schema name

## Value

A data frame with query results

## Examples

``` r
if (FALSE) { # \dontrun{
# Select specific columns
sb_db_query("users", columns = c("id", "name"))

# Filter with where clause
sb_db_query("users", where = list(status = "active"))

# Filter with operators
sb_db_query("orders", where = list(
  status = "pending",
  total = list(gte = 100)
))

# Available operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is

# Raw SQL query
sb_db_query(sql = "SELECT COUNT(*) FROM users WHERE status = 'active'")
} # }
```
