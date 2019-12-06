module Day1
    ( solve
    ) where

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