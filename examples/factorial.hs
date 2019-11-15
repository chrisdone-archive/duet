data N = S N | Z | M N N
sub = \n -> case n of
              S c -> c
fac = \n -> case n of
              Z -> S Z
              _ -> M n (fac (sub n))

facAcc = \a n ->
  case n of
    Z -> a
    _ -> facAcc (M n a) (sub n)

facA = facAcc (S Z)
id = \x -> x
main = fac (S (S Z))
