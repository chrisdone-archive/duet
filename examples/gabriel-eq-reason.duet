data IO a = Print Nat (IO a) | Return a

data Nat = Z | S Nat

data List a = Nil | Cons a (List a)

data Unit = Unit

bind =
  \m f ->
    case m of
      Return a -> f a
      Print bool m1 -> Print bool (bind m1 f)

next = \m n ->
  bind m (\_ -> n)

print = \x -> Print x (Return Unit)

return = Return

repeat = \x -> Cons x (repeat x)

foldr = \cons nil l ->
  case l of
    Nil -> nil
    Cons x xs -> cons x (foldr cons nil xs)

sequence_ = \ms -> foldr next (return Unit) ms

take =
  \n l ->
    case n of
      Z -> Nil
      S m ->
        case l of
          Nil -> Nil
          Cons x xs -> Cons x (take m xs)

replicate = \n x -> take n (repeat x)

replicateM_ = \n m -> sequence_ (replicate n m)

main = replicateM_ (S (S (S (S (S (S Z)))))) (print (S Z))
