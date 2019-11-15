data Tuple a b = Tuple a b

ack = \m n ->
  case Tuple m n of
    Tuple 0 n -> n + 1
    Tuple m 0 -> ack (m - 1) 1
    Tuple m n -> ack (m - 1) (ack m (n - 1))

main = ack 4 0
