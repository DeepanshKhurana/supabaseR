# Truncate a table

Truncate a table

## Usage

``` r
sb_db_truncate(table = NULL, schema = get_schema())
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
# Remove all rows from a table (use with caution!)
sb_db_truncate("temp_data")
} # }
```
