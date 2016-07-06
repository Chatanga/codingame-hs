module Codingame.Misc
    ( (&) -- As of GHC 7.10 (base 4.8.0.0), & is in Data.Function.
    ) where

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x
