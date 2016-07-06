module Epsilon
    ( (|>)
	, epsilon
    ) where

import Delta.Zeta

infixl 8 |>
x |> y = y $ x

epsilon = putStrLn "epsilon"

