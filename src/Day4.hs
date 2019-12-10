module Day4
    ( solveDay4
    )
where

import qualified Data.List                     as List

data SearchState = SearchState
    { lowerBound :: Int
    , upperBound :: Int
    , current :: Int
    , solutions :: Int
    }

mkSearchState :: Int -> Int -> Int -> Int -> SearchState
mkSearchState = SearchState

solveDay4 :: IO ()
solveDay4 = do
    let res = solve 240920 789857
    print res

solve :: Int -> Int -> Int
solve lowerBound upperBound =
    solve' (mkSearchState lowerBound upperBound lowerBound 0)

solve' :: SearchState -> Int
solve' (SearchState lowerBound upperBound current solutions)
    | current == upperBound = solutions
    | otherwise = solve'
        (mkSearchState lowerBound upperBound newCode newSolutions)
  where
    newSolutions =
        if checkCode (digits current) then solutions + 1 else solutions
    newCode = current + 1

checkCode :: [Int] -> Bool
checkCode code = (checkIncresing code) && (checkDouble code)

checkIncresing :: (Ord a) => [a] -> Bool
checkIncresing []           = True
checkIncresing [x         ] = True
checkIncresing (x : y : xs) = x <= y && checkIncresing (y : xs)

checkDouble :: (Eq a) => [a] -> Bool
checkDouble []           = False
checkDouble [x         ] = False
checkDouble (x : y : xs) = x == y || checkDouble (y : xs)

digits :: Int -> [Int]
digits = map (read . (: [])) . show
