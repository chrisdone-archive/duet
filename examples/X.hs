class X a where
 f :: a -> D
data D = D | C
instance X D where
 f = \x -> case x of
             D -> D
             C -> f D

demo = f C
