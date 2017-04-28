data Maybe a = Just a | Nothing
data Either a b = Left a | Right b
data X = X (Either (Maybe Bool) Bool)
data Y = Y

compose = \f g x -> f (g x)
id = \x -> x
and = \x y -> if x
                 then if y
                         then True
                         else False
                 else False
main = Just (if True
                then "ok!"
                else "nope")
