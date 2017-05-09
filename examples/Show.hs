data Tuple a b = Tuple a b
foo = \x -> show x
main = Tuple (foo 1) (foo 'a')
