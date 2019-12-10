module Day3
    ( solveDay3
    )
where

import           System.IO
import           System.Environment
import           Data.List.Split
import           Data.List
import qualified Data.Set                      as Set

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

type Coordinate = (Int, Int)
type GraphCoordinates = Set.Set Coordinate
type Action = (Int, Int) -> (Int, Int)
type CommandList = [String]

solveDay3 :: IO ()
solveDay3 = do
    let path = "input/day3.txt"
    lines <- fmap Text.lines (Text.readFile path)

    let graphA = execute (splitOn "," (Text.unpack (lines !! 0)))
    let graphB = execute (splitOn "," (Text.unpack (lines !! 1)))

    let final  = distance graphA graphB
    print final


distance :: GraphCoordinates -> GraphCoordinates -> Maybe Int
distance graphA graphB = Set.lookupMin allDistancees
  where
    allDistancees = Set.map (\(x, y) -> (abs x) + (abs y)) crossPoints
    crossPoints   = graphA `Set.intersection` graphB

execute :: CommandList -> GraphCoordinates
execute commands = execute' commands (0, 0) Set.empty

execute' :: CommandList -> Coordinate -> GraphCoordinates -> GraphCoordinates
execute' []       currentPoint result = result
execute' commands currentPoint result = execute' (tail commands) newPoint graph
  where
    (newPoint, graph) = executeCommand (head commands) currentPoint result

executeCommand
    :: String
    -> Coordinate
    -> GraphCoordinates
    -> (Coordinate, GraphCoordinates)
executeCommand (direction : amount) currentPoint currentList =
    case direction of
        'U' -> expandUp (read amount) currentPoint currentList
        'R' -> expandRight (read amount) currentPoint currentList
        'D' -> expandDown (read amount) currentPoint currentList
        'L' -> expandLeft (read amount) currentPoint currentList

expandRight
    :: Int -> Coordinate -> GraphCoordinates -> (Coordinate, GraphCoordinates)
expandRight command currentPoint currentList =
    expand command currentPoint currentList (\(x, y) -> (x + 1, y))

expandLeft
    :: Int -> Coordinate -> GraphCoordinates -> (Coordinate, GraphCoordinates)
expandLeft command currentPoint currentList =
    expand command currentPoint currentList (\(x, y) -> (x - 1, y))

expandUp
    :: Int -> Coordinate -> GraphCoordinates -> (Coordinate, GraphCoordinates)
expandUp command currentPoint currentList =
    expand command currentPoint currentList (\(x, y) -> (x, y + 1))

expandDown
    :: Int -> Coordinate -> GraphCoordinates -> (Coordinate, GraphCoordinates)
expandDown command currentPoint currentList =
    expand command currentPoint currentList (\(x, y) -> (x, y - 1))

expand
    :: Int
    -> Coordinate
    -> GraphCoordinates
    -> Action
    -> (Coordinate, GraphCoordinates)
expand command (x, y) currentGraph action
    | command == 0 = (newPoint, currentGraph)
    | otherwise = expand (command - 1)
                         newPoint
                         (Set.insert newPoint currentGraph)
                         action
    where newPoint = action (x, y)
