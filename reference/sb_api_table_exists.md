# Check if a table exists via API

Check if a table exists via API

## Usage

``` r
sb_api_table_exists(table = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- schema:

  The schema name

## Value

`TRUE` if the table exists, `FALSE` otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

sb_api_table_exists("users")

sb_api_table_exists("nonexistent_table")
} # }
```
