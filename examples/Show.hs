data Tuple a b = Tuple a b
thing = \_ -> show 'a'
blah = \x -> thing x
foo = \x y -> Tuple (show y) (show x)
demo = foo 1 'a'
