{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Control.Exception
import Data.Maybe
import Language.Haskell.Exts

import Codingame.WebServices
import Codingame.SourcePackager

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

----------------------------------------------------------------------------------------------------

sourcePackagerTestGroup = testGroup "SourcePackager"
    [   testCase "testCreateMonolithicSourceWithMode" testCreateMonolithicSourceWithMode
    ]

testCreateMonolithicSourceWithMode :: Assertion
testCreateMonolithicSourceWithMode = do

    let sourceFile = "data/Alpha.hs"

    result <- try (createMonolithicSource sourceFile) :: IO (Either Control.Exception.SomeException String)
    case result of
        Left e -> assertBool "Ok" True
        Right _ -> assertFailure "Ambiguous infix expression error expected."

    -- A proper parsing could require to know the fixities for any
    -- operator defined in a source file and used in another.
    let importedFixities = infixl_ 8 ["|>"]
        fixities' = importedFixities ++ fromMaybe [] (fixities defaultParseMode)
        parseMode = defaultParseMode{ fixities = Just fixities' }

    source <- createMonolithicSourceWithMode parseMode sourceFile
    -- source <- createMonolithicSource sourceFile

    expectedSource <- readFile "data/Expected.hs"

    let trim = unwords . words
    trim expectedSource @=? trim source

----------------------------------------------------------------------------------------------------

webServicesTestGroup = testGroup "SourcePackager"
    [   testCase "testPlay" testPlay
    ]

testPlay = do
    let source = "main = putStrLn \"42\""
    credentials <- readCredentials "credentials.json"
    result <- playInIDE credentials (PastChallengeTitle "coders-strike-back") source [IdeCode, DefaultAi] Nothing
    case result of
        Left error -> assertFailure (show error)
        Right GameResult{} -> return ()
    return ()

----------------------------------------------------------------------------------------------------

main = defaultMain
    [ testGroup "Codingame"
        [   sourcePackagerTestGroup
        ,   webServicesTestGroup
        ]
    ]

-- main = putStrLn "" >> exploreIO "../codingame-hs-test/src/Main.hs"
