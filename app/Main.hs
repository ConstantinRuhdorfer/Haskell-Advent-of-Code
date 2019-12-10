module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

import           System.IO
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            print
                "Please give atleast one command line argument like: [all|day[1|...|X]]"
        [arg] -> handleArg arg
        _     -> error "too many arguments just 1 supported."

{-|
  The 'handleArg' function gets a arg and does something with it.
-}
handleArg :: String -> IO ()
handleArg arg = case arg of
    "day1" -> Day1.solveDay1
    "day2" -> Day2.solveDay2
    "day3" -> Day3.solveDay3
    "day4" -> Day4.solveDay4
    "all"  -> do
        Day1.solveDay1
        Day2.solveDay2
        Day3.solveDay3
        Day4.solveDay4
    _ -> print "Not supported try [all|day[1|...|X]]"
