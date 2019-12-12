module Day2
    ( solveDay2
    )
where

import           System.IO
import           System.Environment
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

-- desired was 1969072
solveDay2 :: IO ()
solveDay2 =
    print "Executing programm in input/day2.txt with 12 and 2"
        >>  fmap Text.lines (Text.readFile "input/day2.txt")
        >>= \inputList -> print (solve inputList 12 2)
                >> print (solver inputList 19690720 99 99)


solve :: [Text.Text] -> Int -> Int -> Int
solve inputProgram noun verb =
    (firstElem . execute . (initialise noun verb) . strToInt) inputProgram

solver :: [Text.Text] -> Int -> Int -> Int -> [Int]
solver inputProgram desired noun verb
    | result /= desired && verb /= 0 = solver inputProgram
                                              desired
                                              noun
                                              (verb - 1)
    | result < desired || (result /= desired && verb == 0 && noun /= 0) = solver
        inputProgram
        desired
        (noun - 1)
        99
    | otherwise = [noun, verb]
    where result = solve inputProgram noun verb

firstElem :: [Int] -> Int
firstElem list = list !! 0

initialise :: Int -> Int -> [Int] -> [Int]
initialise noun verb (a : b : c : list) = a : noun : verb : list

execute :: [Int] -> [Int]
execute list = execute' list 0

execute' :: [Int] -> Int -> [Int]
execute' list currentIndex
    | list !! currentIndex == 99 = list
    | list !! currentIndex == 1 = execute' (executeAdd list currentIndex)
                                           (currentIndex + 4)
    | list !! currentIndex == 2 = execute' (executeMult list currentIndex)
                                           (currentIndex + 4)

executeAdd :: [Int] -> Int -> [Int]
executeAdd list currentIndex = handle list currentIndex (\x y -> x + y)

executeMult :: [Int] -> Int -> [Int]
executeMult list currentIndex = handle list currentIndex (\x y -> x * y)

handle :: [Int] -> Int -> (Int -> Int -> Int) -> [Int]
handle list currentIndex action = replaceNth
    storeAdress
    (action (list !! aAdress) (list !! bAdress))
    list
  where
    aAdress     = list !! (currentIndex + 1)
    bAdress     = list !! (currentIndex + 2)
    storeAdress = list !! (currentIndex + 3)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs) | n == 0    = newVal : xs
                             | otherwise = x : replaceNth (n - 1) newVal xs

strToInt :: [Text.Text] -> [Int]
strToInt = map (read . Text.unpack)
