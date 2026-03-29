# Update rows in a table

Update rows in a table

## Usage

``` r
sb_update(table = NULL, data = NULL, where = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- data:

  A named list of column = value pairs to set

- where:

  A named list for WHERE clause

- schema:

  The schema name

## Value

Number of rows affected (invisibly)
