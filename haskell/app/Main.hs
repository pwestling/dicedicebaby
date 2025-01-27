module Main where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode, encode)
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    processInput

processInput :: IO ()
processInput = do
    input <- getLine
    case decode (B.pack input) of
        Nothing -> putStrLn "{\"success\": false, \"error\": \"Invalid JSON\"}"
        Just testCase -> do
            -- TODO: Process test case and calculate probabilities
            putStrLn "{\"success\": true}"
    processInput 