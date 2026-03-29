# Get table schema

Get table schema

## Usage

``` r
sb_db_schema(table = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- schema:

  The schema name

## Value

A data frame with column_name and data_type

## Examples

``` r
if (FALSE) { # \dontrun{
# Get column info for a table
sb_db_schema("users")

# Returns: column_name, data_type, is_nullable
} # }
```
