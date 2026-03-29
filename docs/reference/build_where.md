# Build WHERE clause from list

Build WHERE clause from list

## Usage

``` r
build_where(where, conn, include_keyword = TRUE)
```

## Arguments

- where:

  A named list of conditions

- conn:

  Database connection for escaping

- include_keyword:

  Include "WHERE" keyword (default TRUE)

## Value

SQL string for WHERE clause

## Details

Supports operators via nested lists:

- `list(id = 1)` → `id = 1`

- `list(age = list(gt = 25))` → `age > 25`

- `list(name = list(like = "A%"))` → `name LIKE 'A%'`

Supported operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is
