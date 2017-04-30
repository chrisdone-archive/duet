data Either a b = Left a | Right b
foo =
  \x ->
    case x of
     Left k -> k
     Right l -> "Right..."
main = foo (Left (if True
                     then "Foo."
                     else "Bar!"))

data Pair a b = Pair a b
singleton = \x -> Pair x x
main1 = case singleton True of
          Pair True True -> "OK!"
          Pair True False -> "t/f"
          Pair False _ -> "faux/?"
          _ -> "..."
