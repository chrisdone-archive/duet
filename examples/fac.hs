factorial = \n -> case n of
                    0 -> 1
                    1 -> 1
                    _ -> n * factorial (n - 1)


go =
  \n acc ->
    case n of
      0 -> 1
      1 -> 1
      _ -> go (n - 1) (n * acc)

it = go 5 1
