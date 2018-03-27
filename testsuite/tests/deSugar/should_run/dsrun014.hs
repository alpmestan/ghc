{-# LANGUAGE UnboxedTuples #-}

module Main where

import Debug.Trace
import GHC.Conc

{-# NOINLINE f #-}
f :: a -> b -> (# a,b #)
f x y = x `pseq` y `pseq` (# x,y #)

g :: Int -> Int -> Int
g v w = case f v w of
          (# a,b #) -> a+b

main = print (g (trace "one" 1) (trace "two" 2))
-- The args should be evaluated in the right order!
