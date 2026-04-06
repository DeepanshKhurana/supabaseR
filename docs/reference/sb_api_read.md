# Read table data via API

Read table data via API

## Usage

``` r
sb_api_read(table = NULL, limit = 0, schema = get_schema())
```

## Arguments

- table:

  The table name

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

# Read all rows
sb_api_read("users")

# Read with a row limit
sb_api_read("users", limit = 10)

# Read from a non-default schema
sb_api_read("orders", schema = "billing")
} # }
```
