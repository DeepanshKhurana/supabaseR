# Query a table

Query a table

## Usage

``` r
sb_query(
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

  The table name

- columns:

  Columns to select

- where:

  A named list for WHERE clause

- limit:

  Maximum rows to return

- sql:

  Raw SQL query (db backend only)

- schema:

  The schema name

## Value

A data frame with query results
