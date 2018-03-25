
module Main where

import Lib

main :: IO ()
main = do
  print (_1 <+> _0 :: Bool)
  print (_1 <+> _0 :: Prob)
  print (_1 <+> _0 :: Log)
  print (_1 <+> _0 :: Tropical)
  print arc
  -- print (Transition arc)
