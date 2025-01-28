{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (decode, encode)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout, stderr, hPutStrLn)
import Control.Monad (forever)
import Control.Exception (catch, SomeException)

import Probability.Warhammer.Attack

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    BS8.hPutStrLn stderr "Calculator process started"
    forever processLine
  where
    processLine = do
        line <- BS8.hGetLine stdin
        case decode (LBS.fromStrict line) of
            Nothing -> do
                BS8.hPutStrLn stderr $ "Failed to parse JSON: " <> line
                undefined
            Just input -> do
                BS8.hPutStrLn stderr "Running test case"
                let (profile, defender) = processTestInput input
                    results = simulateAttacks profile defender []
                    collapsed = collapseStages results
                    validation = validateResults collapsed input.expected
                hPutStrLn stderr  $ "Final results: " <> show collapsed
                LBS.putStr $ encode $ resultsToOutput results validation
                BS8.putStrLn "" 