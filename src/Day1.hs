module Day1
  ( solveDay1
  )
where

import           System.IO
import           System.Environment
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

solveDay1 :: IO ()
solveDay1 =
  print "Calculating fuel for input file input/day1.txt"
    >>  fmap Text.lines (Text.readFile "input/day1.txt")
    >>= \inputList -> print $ solve inputList

solve :: [Text.Text] -> Int
solve = calcFuel . strToInt

calcFuel :: [Int] -> Int
calcFuel list = (sum (map (\x -> calcFuel' x 0) list))

calcFuel' :: Int -> Int -> Int
calcFuel' fuel res | additionalFuel <= 0 = res
                   | otherwise = calcFuel' additionalFuel (res + additionalFuel)
  where additionalFuel = subtract 2 (fuel `quot` 3)

strToInt :: [Text.Text] -> [Int]
strToInt = map (read . Text.unpack)
