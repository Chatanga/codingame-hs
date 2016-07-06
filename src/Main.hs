{-# LANGUAGE CPP, TemplateHaskell #-}

module Main
    ( main
    ) where

import Data.Maybe
import Language.Haskell.Exts

import Test.Framework

import Codingame.WebServices
import Codingame.SourcePackager

----------------------------------------------------------------------------------------------------

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION testMain
#endif
main = MAIN_FUNCTION

----------------------------------------------------------------------------------------------------

-- Entry point for unit tests.
testMain :: IO ()
testMain = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Codingame"
        [   sourcePackagerTestGroup
        ]
    ]
