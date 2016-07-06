module Codingame.Debug
    ( trace
    , _trace
    , traceList
    , _traceList
) where

import Data.List
import qualified Debug.Trace as Trace

debug = True

trace :: Show a => String -> a -> a
trace t a = if debug
    then Trace.trace (t ++ " = " ++ show a) a
    else a
_trace t = id

traceList :: Show a => String -> [a] -> [a]
traceList t as = if debug
    then Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (fmap show as) ++ "\n]") as
    else as
_traceList t = id
