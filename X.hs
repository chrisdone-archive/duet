compose = \f g x -> f (g x)
id = \x -> x
and = \x y -> if x
                 then if y
                         then True
                         else False
                 else False
main = and True True
