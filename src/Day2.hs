module Day2
    ( solver
    ) where

-- desired was 19690720

solve :: [String] -> Int -> Int -> Int
solve inputProgram noun verb
    = (firstElem.execute.(initialise noun verb).strToInt) inputProgram

solver :: [String] -> Int -> Int -> Int -> [Int] 
solver inputProgram desired noun verb
    | result /= desired && verb /= 0 = solver inputProgram desired noun (verb-1)
    | result /= desired && verb == 0 && noun /= 0 = solver inputProgram desired (noun-1) 99
    | otherwise = [noun, verb]
  where
    result = solve inputProgram noun verb

firstElem :: [Int] -> Int
firstElem list = list!!0

initialise :: Int -> Int -> [Int] -> [Int] 
initialise noun verb (a:b:c:list) = a:noun:verb:list

execute :: [Int] -> [Int]
execute list = execute' list 0

execute' :: [Int] -> Int -> [Int]
execute' list currentIndex
    | list!!currentIndex == 99 = list
    | list!!currentIndex == 1 = execute' (executeAdd list currentIndex) (currentIndex+4)
    | list!!currentIndex == 2 = execute' (executeMult list currentIndex) (currentIndex+4)

executeAdd :: [Int] -> Int -> [Int]
executeAdd list currentIndex = handle list currentIndex (\x y -> x + y)

executeMult :: [Int] -> Int -> [Int]
executeMult list currentIndex = handle list currentIndex (\x y -> x * y)

handle :: [Int] -> Int -> (Int -> Int -> Int) -> [Int]
handle list currentIndex action
    = replaceNth storeAdress (action (list!!aAdress) (list!!bAdress)) list
  where
    aAdress = list!!(currentIndex+1)
    bAdress = list!!(currentIndex+2)
    storeAdress = list!!(currentIndex+3)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

strToInt ::  [String] -> [Int]
strToInt list = map read list