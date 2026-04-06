# Build PostgREST URL query parameters from a where list

Build PostgREST URL query parameters from a where list

## Usage

``` r
.build_api_params(columns = "*", where = NULL, limit = 0)
```

## Arguments

- columns:

  Columns to select: "\*" or a character vector

- where:

  Named list of filter conditions; same operator vocabulary as
  [`build_where()`](https://deepanshkhurana.github.io/supabaseR/reference/build_where.md):
  bare value for equality, or `list(op = val)` for operators `eq`,
  `neq`, `gt`, `gte`, `lt`, `lte`, `like`, `ilike`, `in`, `is`

- limit:

  Maximum rows to return; 0 means no limit

## Value

Named list suitable for `httr2::req_url_query(!!!params)`
