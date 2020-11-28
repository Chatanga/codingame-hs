{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Beta
    ( beta
    ) where

import Delta.Epsilon
import Delta.Zeta

beta (!x, !y) = putStrLn $ "beta" ++ show x ++ show y

