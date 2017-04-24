compose = \f g x -> f (g x)
id = \x -> x
main = id ((compose id id) "a")
