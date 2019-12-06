module Main where

import Day2

import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->    print "Please give atleast one command line argument like: ... input/bla.txt"
        [arg] -> handleFilePath arg 
        _ ->     error "too many arguments just 1 supported."

{-|
  The 'handleFilePath' function gets a file path and does something with it.
-}
handleFilePath :: String -> IO ()
handleFilePath path = do
    let list = []
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = singlewords

    let res = solver list 19690720 99 99

    print res

    hClose handle