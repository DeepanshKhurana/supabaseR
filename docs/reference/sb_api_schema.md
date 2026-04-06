# Get table schema via API

Queries the PostgREST OpenAPI spec to retrieve column names and types
for a table.

## Usage

``` r
sb_api_schema(table = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- schema:

  The schema name

## Value

A `tibble` with columns `column_name` and `data_type`

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# Get column names and types for a table
sb_api_schema("users")

# Get schema for a table in a non-default schema
sb_api_schema("orders", schema = "billing")
} # }
```
