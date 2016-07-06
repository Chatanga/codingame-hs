module Alpha
    ( runMain
	, other
    ) where

import Beta
import Gamma
import Delta.Epsilon
import Delta.Zeta

runMain = do
    putStrLn (show other)

other = [1, 2, 3, 4, 5]
    |> head . groupBy (\a b -> a `mod` 2 == b `mod` 2)
    |> sum

