import Gamma
import Delta.Zeta
main = do putStrLn (show other)
other
  = [1, 2, 3, 4, 5] |>
      head . groupBy (\ a b -> a `mod` 2 == b `mod` 2)
      |> sum
beta = putStrLn "beta"

infixl 8 |>
x |> y = y $ x
epsilon = putStrLn "epsilon"

