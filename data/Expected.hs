{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}
import Gamma
import Delta.Zeta
main = do putStrLn (show other)
other
  = [1, 2, 3, 4, 5] |>
      head . groupBy (\ a b -> a `mod` 2 == b `mod` 2)
      |> sum
beta (!x, !y) = putStrLn $ "beta" ++ show x ++ show y

infixl 8 |>
x |> y = y $ x
epsilon = putStrLn "epsilon"
