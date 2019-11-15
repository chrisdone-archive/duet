go = \n res ->
  case n of
    0 -> 1
    n -> go (n - 1) (res * n)

fac = \n -> go n 1

factorial = \n ->
  case n of
    0 -> 1
    n -> n * factorial (n - 1)

main = fac 5
