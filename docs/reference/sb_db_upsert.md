# Upsert rows into a table

Insert rows, or update if conflict on specified columns.

## Usage

``` r
sb_db_upsert(
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
# Upsert: insert or update on conflict
sb_db_upsert(
  "users",
  data = data.frame(id = 1, name = "Alice", email = "alice@new.com"),
  conflict_columns = "id"
)

# Upsert multiple rows with composite key
sb_db_upsert(
  "order_items",
  data = data.frame(
    order_id = c(1, 1),
    product_id = c(10, 20),
    quantity = c(5, 3)
  ),
  conflict_columns = c("order_id", "product_id")
)
} # }
```
