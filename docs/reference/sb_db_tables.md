# List tables in schema

List tables in schema

## Usage

``` r
sb_db_tables(schema = get_schema())
```

## Arguments

- schema:

  The schema name

## Value

A character vector of table names

## Examples

``` r
if (FALSE) { # \dontrun{
# List all tables in current schema
sb_db_tables()

# List tables in a specific schema
sb_db_tables(schema = "auth")
} # }
```
