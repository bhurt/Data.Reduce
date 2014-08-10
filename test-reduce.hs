module Main where

import System.Exit

main :: IO ()
main = do
    putStrLn "This test always fails!"
    exitFailure

