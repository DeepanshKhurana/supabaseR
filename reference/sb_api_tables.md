# List tables via API

Queries the PostgREST OpenAPI spec to list exposed tables.

## Usage

``` r
sb_api_tables(schema = get_schema())
```

## Arguments

- schema:

  The schema name

## Value

A character vector of table names

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# List all tables in the default schema
sb_api_tables()

# List tables in a specific schema
sb_api_tables(schema = "billing")
} # }
```
