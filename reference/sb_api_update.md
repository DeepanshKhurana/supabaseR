# Update rows in a table via API

Update rows in a table via API

## Usage

``` r
sb_api_update(table = NULL, data = NULL, where = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- data:

  A named list of column = value pairs to set

- where:

  A named list for filtering. Supports operators via nested lists.

- schema:

  The schema name

## Value

Number of rows updated (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# Update a row by id
sb_api_update(
  "users",
  data  = list(email = "newemail@example.com"),
  where = list(id = 1)
)

# Update with an operator
sb_api_update(
  "products",
  data  = list(in_stock = FALSE),
  where = list(quantity = list(lte = 0))
)
} # }
```
