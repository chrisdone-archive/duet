data Tuple a b = Tuple a b
foo = \x -> show x
demo = Tuple (foo 1) (foo 'a')
