# Read table data

Read table data

## Usage

``` r
sb_db_read(table = NULL, limit = 0, schema = get_schema())
```

## Arguments

- table:

  The table name

- limit:

  Maximum rows to return (0 for all)

- schema:

  The schema name

## Value

A data frame with table data

## Examples

``` r
if (FALSE) { # \dontrun{
# Read all rows from a table
users <- sb_db_read("users")

# Read with a row limit
recent <- sb_db_read("orders", limit = 100)

# Read from a specific schema
products <- sb_db_read("products", schema = "inventory")
} # }
```
