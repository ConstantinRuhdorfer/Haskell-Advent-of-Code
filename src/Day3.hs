module Day3
    ( solveDay3
    )
where

import           System.IO
import           System.Environment
import           Data.List.Split
import           Data.List
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

solveDay3 :: IO ()
solveDay3 =
    print "Executing programm in input/day3.txt"
        >>  print
                "Finding the distance of the intersection with the closest distance to (0,0):"
        >>  fmap Text.lines (Text.readFile "input/day3.txt")
        >>= \inputList -> print
                (shortestPathIntersection
                    (execute $ unpack $ inputList !! 0)
                    (execute $ unpack $ inputList !! 1)
                )


{-| 
    Solutions
-}
type Coordinate = (Int, Int)
type GraphCoordinates = Set.Set Coordinate
type Action = (Int, Int) -> (Int, Int)
type CommandList = [String]
type DistancesToStart = Map.Map Coordinate Int

data GraphState = GraphState
    { coordinates :: GraphCoordinates
    , endPoint :: Coordinate
    , distancesToStart :: DistancesToStart
    , longestPath :: Int
    }

mkGraphState
    :: GraphCoordinates -> Coordinate -> DistancesToStart -> Int -> GraphState
mkGraphState = GraphState

shortestPathIntersection :: GraphState -> GraphState -> Int
shortestPathIntersection graphA graphB = combineAndReduce
    crossPointDistancesA
    crossPointDistancesB
    []
  where
    crossPoints =
        Set.elems ((coordinates graphA) `Set.intersection` (coordinates graphB))
    crossPointDistancesA = map
        (\c -> (\(Just i) -> i) (Map.lookup c (distancesToStart graphA)))
        crossPoints
    crossPointDistancesB = map
        (\c -> (\(Just i) -> i) (Map.lookup c (distancesToStart graphB)))
        crossPoints

combineAndReduce :: [Int] -> [Int] -> [Int] -> Int
combineAndReduce list1 list2 result
    | null list1 && null list2 = minimum result
    | otherwise = combineAndReduce (tail list1)
                                   (tail list2)
                                   (newElement : result)
    where newElement = (head list1) + (head list2)

distance :: GraphCoordinates -> GraphCoordinates -> Maybe Int
distance graphA graphB = Set.lookupMin allDistancees
  where
    allDistancees = Set.map (\(x, y) -> (abs x) + (abs y)) crossPoints
    crossPoints   = graphA `Set.intersection` graphB

execute :: CommandList -> GraphState
execute commands =
    execute' commands (mkGraphState Set.empty (0, 0) Map.empty 0)

execute' :: CommandList -> GraphState -> GraphState
execute' []       state = state
execute' commands state = execute' (tail commands) newState
    where newState = executeCommand (head commands) state

executeCommand :: String -> GraphState -> GraphState
executeCommand (direction : amount) state = case direction of
    'U' -> expandUp (read amount) state
    'R' -> expandRight (read amount) state
    'D' -> expandDown (read amount) state
    'L' -> expandLeft (read amount) state

expandRight :: Int -> GraphState -> GraphState
expandRight command state = expand command state (\(x, y) -> (x + 1, y))

expandLeft :: Int -> GraphState -> GraphState
expandLeft command state = expand command state (\(x, y) -> (x - 1, y))

expandUp :: Int -> GraphState -> GraphState
expandUp command state = expand command state (\(x, y) -> (x, y + 1))

expandDown :: Int -> GraphState -> GraphState
expandDown command state = expand command state (\(x, y) -> (x, y - 1))

expand :: Int -> GraphState -> Action -> GraphState
expand command (GraphState coordinates endPoint distances longest) action
    | command == 0 = mkGraphState coordinates endPoint distances longest
    | otherwise    = expand (command - 1) newState action
  where
    newPoint       = action endPoint
    newDistances   = Map.insert newPoint (longest + 1) distances
    newCoordinates = Set.insert newPoint coordinates
    newState = mkGraphState newCoordinates newPoint newDistances (longest + 1)


unpack :: Text.Text -> [String]
unpack input = splitOn "," $ Text.unpack input
