factorial = \n -> case n of
                    0 -> 1
                    1 -> 1
                    _ -> n * factorial (n - 1)


go =
  \n acc0 ->
    case acc0 of
      acc ->
        case n of
          0 -> acc
          1 -> acc
          _ -> go (n - 1) (n * acc)

go_efficient =
  \n acc0 ->
    case acc0 of
      !acc ->
        case n of
          0 -> acc
          1 -> acc
          nf -> go_efficient (nf - 1) (nf * acc)

it = go 5 1

it_efficient = go_efficient 5 1
