module Day1
    ( solveDay1
    ) where
    
import System.IO
import System.Environment

solveDay1 :: IO ()
solveDay1 = do
  let list = []
  let path = "input/day1.txt"
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
      list = singlewords

  let res = solve list

  print ("Calculating fuel for input file " ++ path)
  print res

  hClose handle

solve :: [String] -> Int
solve input = (calcFuel.strToInt) input

calcFuel :: [Int] -> Int
calcFuel list = (sum (map (\x -> calcFuel' x 0) list))

calcFuel' :: Int -> Int -> Int 
calcFuel' fuel res
    | additionalFuel <= 0 = res
    | otherwise = calcFuel' additionalFuel (res + additionalFuel)
  where
    additionalFuel = subtract 2 (fuel `quot` 3) 

strToInt ::  [String] -> [Int]
strToInt list = map read list