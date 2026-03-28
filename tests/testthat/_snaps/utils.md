# build_where() / should error on unknown operator

    Code
      build_where(list(x = list(unknown = 1)), mock_conn)
    Condition
      Error:
      ! Unknown operator: unknown

