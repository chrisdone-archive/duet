compose = \f g x -> f (g x)
id = \x -> x
demo = id (if id True
              then "true"
              else id id "false")
