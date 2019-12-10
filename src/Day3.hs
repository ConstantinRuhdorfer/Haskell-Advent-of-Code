module Day3
    ( solveDay3
    )
where

import           System.IO
import           System.Environment
import           Data.List.Split
import           Data.List
import qualified Data.Set                      as Set
import qualified Data.Map.Lazy                 as Map

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

solveDay3 :: IO ()
solveDay3 = do
    let path = "input/day3.txt"
    lines <- fmap Text.lines (Text.readFile path)

    let graphA = execute (splitOn "," (Text.unpack (lines !! 0)))
    let graphB = execute (splitOn "," (Text.unpack (lines !! 1)))

    let final  = distance (coordinates graphA) (coordinates graphB)
    print final

{-| 
    Solutions
-}
type Coordinate = (Int, Int)
type GraphCoordinates = Set.Set Coordinate
type Action = (Int, Int) -> (Int, Int)
type CommandList = [String]
type DistancesToStart = Map.Map (Coordinate -> Int)

data GraphState = GraphState
    { coordinates :: GraphCoordinates
    , endPoint :: Coordinate
    }

mkGraphState :: GraphCoordinates -> Coordinate -> GraphState
mkGraphState coordinates endPoint = GraphState coordinates endPoint

distance :: GraphCoordinates -> GraphCoordinates -> Maybe Int
distance graphA graphB = Set.lookupMin allDistancees
  where
    allDistancees = Set.map (\(x, y) -> (abs x) + (abs y)) crossPoints
    crossPoints   = graphA `Set.intersection` graphB

execute :: CommandList -> GraphState
execute commands = execute' commands (mkGraphState Set.empty (0, 0))

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
expand command (GraphState coordinates endPoint) action
    | command == 0 = mkGraphState coordinates newPoint
    | otherwise    = expand (command - 1) newState action
  where
    newPoint = action endPoint
    newState = mkGraphState (Set.insert newPoint coordinates) newPoint
