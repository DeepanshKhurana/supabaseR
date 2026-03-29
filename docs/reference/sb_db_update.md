# Update rows in a table

Update rows in a table

## Usage

``` r
sb_db_update(table = NULL, data = NULL, where = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- data:

  A named list of column = value pairs to set

- where:

  A named list for WHERE clause. Supports operators via nested lists.

- schema:

  The schema name

## Value

Number of rows affected (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
# Update a row by id
sb_db_update(
  "users",
  data = list(email = "newemail@example.com"),
  where = list(id = 1)
)

# Update with operator
sb_db_update(
  "products",
  data = list(in_stock = FALSE),
  where = list(quantity = list(lte = 0))
)
} # }
```
