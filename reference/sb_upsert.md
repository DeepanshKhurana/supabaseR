# Upsert rows into a table

Upsert rows into a table

## Usage

``` r
sb_upsert(
  table = NULL,
  data = NULL,
  conflict_columns = NULL,
  schema = get_schema()
)
```

## Arguments

- table:

  The table name

- data:

  A data frame of rows to upsert

- conflict_columns:

  Column(s) to check for conflicts

- schema:

  The schema name

## Value

Number of rows affected (invisibly)
