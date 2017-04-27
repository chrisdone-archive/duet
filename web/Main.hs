{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Snap
import qualified Snappy

main :: IO ()
main = do
  element <- Snap.getElementById "app"
  snap <- Snap.new element
  rec c <-
        Snappy.circle
          snap
          (Snappy.draggable c 50 Snappy.dragDX)
          (Snappy.draggable c 45 Snappy.dragDY)
          (pure 20)
  _ <-
    Snappy.text
      snap
      (pure 10)
      (pure 20)
      (Snappy.zipDynamics
         (\x y -> "(x,y) = " ++ show (x,y))
         (Snappy.circleX c)
         (Snappy.circleY c))
  pure ()
