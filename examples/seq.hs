seq :: a -> b -> b
seq =
  \x y ->
    case x of
      !_ -> y
loop = loop
main = seq loop 1
