module Day4
    ( solveDay4
    )
where

-- 750
import qualified Data.List                     as List
import           Debug.Trace

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
    -- let res = solve 22456 22457
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
checkCode code = checkIncresing code && (checkNeighbours code 0)

checkIncresing :: (Ord a) => [a] -> Bool
checkIncresing []           = True
checkIncresing [x         ] = True
checkIncresing (x : y : xs) = x <= y && checkIncresing (y : xs)

{-|
    Pain... so much pain.
    (Aka. there is a better solution but I didnt feel motivated).
-}
checkNeighbours :: (Eq a) => [a] -> Int -> Bool
checkNeighbours input i
    | i == 0
    = (input !! i == input !! (i + 1) && input !! (i + 1) /= input !! (i + 2))
        || checkNeighbours input (i + 1)
    | i == ((length input) - 3)
    = (input !! i /= input !! (i + 1) && input !! (i + 1) == input !! (i + 2))
        || (  input
           !! (i - 1)
           /= input
           !! i
           && input
           !! i
           == input
           !! (i + 1)
           && input
           !! (i + 1)
           /= input
           !! (i + 2)
           )
    | otherwise
    = (  input
      !! (i - 1)
      /= input
      !! i
      && input
      !! i
      == input
      !! (i + 1)
      && input
      !! (i + 1)
      /= input
      !! (i + 2)
      )
        || checkNeighbours input (i + 1)



digits :: Int -> [Int]
digits = map (read . (: [])) . show
