module Main where

import qualified Day1
import qualified Day2

import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->    print "Please give atleast one command line argument like: ... input/bla.txt"
        [arg] -> handleArg arg 
        _ ->     error "too many arguments just 1 supported."

{-|
  The 'handleArg' function gets a arg and does something with it.
-}
handleArg :: String -> IO ()
handleArg arg = do
    case arg of
        "day1" -> Day1.solveDay1
        "day2" -> Day2.solveDay2
        "all"  -> do
            Day1.solveDay1
            Day2.solveDay2
        _      -> print "Not supported try day1, day2 or all"