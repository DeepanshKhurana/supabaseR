# Query a table via API

Query a table via API

## Usage

``` r
sb_api_query(
  table = NULL,
  columns = "*",
  where = NULL,
  limit = 0,
  schema = get_schema()
)
```

## Arguments

- table:

  The table name

- columns:

  Columns to select (default: all). Character vector or `"*"`.

- where:

  A named list for filtering. Supports operators via nested lists:
  `list(id = 1)` for equality, `list(age = list(gt = 25))` for
  `age > 25`. Operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is

- limit:

  Maximum rows to return (0 for all)

- schema:

  The schema name

## Value

A `tibble`

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# Select specific columns
sb_api_query("users", columns = c("id", "name"))

# Filter with a simple equality condition
sb_api_query("users", where = list(status = "active"))

# Filter with operators
sb_api_query("orders", where = list(
  status = "pending",
  total = list(gte = 100)
))

# Available operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is

# Limit results
sb_api_query("users", limit = 5)
} # }
```
