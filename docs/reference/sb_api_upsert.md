# Upsert rows into a table via API

Insert rows, or update on conflict with specified columns.

## Usage

``` r
sb_api_upsert(
  table = NULL,
  data = NULL,
  conflict_columns = NULL,
  schema = get_schema()
)
```

## Arguments

- table:

  The table name

- data:

  A data frame of rows to upsert

- conflict_columns:

  Column(s) to check for conflicts (e.g., primary key)

- schema:

  The schema name

## Value

Number of rows affected (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# Upsert a single row (insert or update on conflict)
sb_api_upsert(
  "users",
  data.frame(id = 1, name = "Alice Updated"),
  conflict_columns = "id"
)

# Upsert multiple rows with a composite key
sb_api_upsert(
  "orders",
  data.frame(
    user_id  = c(1, 2),
    order_id = c(10, 11),
    status   = c("shipped", "pending")
  ),
  conflict_columns = c("user_id", "order_id")
)
} # }
```
