# Truncate a table via API

Deletes all rows from the table using a DELETE request with no filter.
Requires the secret key or RLS must be disabled for the table.

## Usage

``` r
sb_api_truncate(table = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- schema:

  The schema name

## Value

Invisible NULL

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# Remove all rows from a table
# Requires a secret key or Row Level Security to be disabled
sb_api_truncate("logs")
} # }
```
