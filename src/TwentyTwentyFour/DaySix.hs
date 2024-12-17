{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module TwentyTwentyFour.DaySix (numberOfPositions, numberOfObstructions) where

import Control.Lens (Ixed (ix), element, (.~))
import Data.List
import Data.Maybe (isJust)
import qualified Data.String as V
import qualified Data.Vector as V
import Debug.Trace (trace, traceShow)
import Flow

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

guardSymbols :: [Char]
guardSymbols = ['>', '<', '^', 'v']

charToDirection :: Char -> Maybe Direction
charToDirection '^' = Just UP
charToDirection 'v' = Just DOWN
charToDirection '<' = Just LEFT
charToDirection '>' = Just RIGHT
charToDirection _ = Nothing

anyCharInString :: String -> String -> Bool
anyCharInString chars str = any (`elem` str) chars

update2d :: Int -> Int -> a -> [[a]] -> [[a]]
update2d x y value listOfLists = (ix y .~ (ix x .~ value) (listOfLists !! y)) listOfLists

get2d :: Int -> Int -> [[a]] -> a
get2d x y list = list !! y !! x

data GuardData = GuardData
  { position :: (Int, Int),
    direction :: Direction
  }

getGuardData :: String -> Maybe GuardData
getGuardData =
  lines
    .> zip [0 ..]
    .> foldl
      ( \outerAcc (y, line) ->
          if isJust outerAcc
            then outerAcc
            else
              foldl
                ( \innerAcc (x, char) ->
                    if isJust innerAcc
                      then innerAcc
                      else
                        if char `elem` guardSymbols
                          then case charToDirection char of
                            Just foundDirection ->
                              Just
                                GuardData
                                  { position = (x, y),
                                    direction = foundDirection
                                  }
                            Nothing -> Nothing
                          else Nothing
                )
                Nothing
                (zip [0 ..] line)
      )
      Nothing

type MazeMap = V.Vector (V.Vector Char)

data Maze = Maze
  { mazeMap :: MazeMap,
    guardData ::
      Maybe GuardData
  }

createMap :: String -> V.Vector (V.Vector Char)
createMap =
  lines
    .> V.fromList
    .> V.map V.fromString
    .> V.map (V.map (\character -> if character `elem` guardSymbols then '.' else character))

createMaze :: String -> Maze
createMaze input =
  Maze
    { mazeMap = createMap input,
      guardData = getGuardData input
    }

applyStep :: MazeMap -> GuardData -> Maze
applyStep mazeMap guardData =
  let (x, y) = position guardData
      guardDirection = direction guardData
   in case guardDirection of
        UP ->
          if y == 0
            then
              Maze
                { mazeMap,
                  guardData = Nothing
                }
            else
              if get2d x (y - 1) maze /= '#'
                then Just (update2d x y 'X' (update2d x (y - 1) '^' maze))
                else Just (update2d x y '>' maze)
        DOWN ->
          if y == length maze
            then Just (update2d x y 'X' maze)
            else
              if get2d x (y + 1) maze /= '#'
                then Just (update2d x y 'X' (update2d x (y + 1) 'v' maze))
                else Just (update2d x y '<' maze)
        LEFT ->
          if x == 0
            then
              Maze
                { mazeMap,
                  guardData = Nothing
                }
            else
              if get2d (x - 1) y maze /= '#'
                then Just (update2d x y 'X' (update2d (x - 1) y '<' maze))
                else Just (update2d x y '^' maze)
        RIGHT ->
          if x == length (head maze)
            then Just (update2d x y 'X' maze)
            else
              if get2d (x + 1) y maze /= '#'
                then Just (update2d x y 'X' (update2d (x + 1) y '>' maze))
                else Just (update2d x y 'v' maze)

applyStepWithObstructions :: ([String], Char, Int) -> Maybe ([String], Char, Int)
applyStepWithObstructions (maze, currentChar, numberOfObstacles) =
  guardPosition maze >>= \(x, y) ->
    guardDirection maze >>= \case
      UP ->
        if y == 0
          then
            Just (update2d x y 'X' maze, currentChar, numberOfObstacles)
          else
            let nextChar = get2d x (y - 1) maze
             in if nextChar /= '#'
                  then Just (update2d x y 'X' (update2d x (y - 1) '^' maze), nextChar, if currentChar == 'X' then numberOfObstacles + 1 else numberOfObstacles)
                  else Just (update2d x y '>' maze, currentChar, numberOfObstacles)
      DOWN ->
        if y == length maze
          then Just (update2d x y 'X' maze, currentChar, numberOfObstacles)
          else
            let nextChar = get2d x (y + 1) maze
             in if nextChar /= '#'
                  then Just (update2d x y 'X' (update2d x (y + 1) 'v' maze), nextChar, if currentChar == 'X' then numberOfObstacles + 1 else numberOfObstacles)
                  else Just (update2d x y '<' maze, currentChar, numberOfObstacles)
      LEFT ->
        if x == 0
          then Just (update2d x y 'X' maze, currentChar, numberOfObstacles)
          else
            let nextChar = get2d (x - 1) y maze
             in if nextChar /= '#'
                  then Just (update2d x y 'X' (update2d (x - 1) y '<' maze), nextChar, if currentChar == 'X' then numberOfObstacles + 1 else numberOfObstacles)
                  else Just (update2d x y '^' maze, currentChar, numberOfObstacles)
      RIGHT ->
        if x == length (head maze)
          then Just (update2d x y 'X' maze, currentChar, numberOfObstacles)
          else
            let nextChar = get2d (x + 1) y maze
             in if nextChar /= '#'
                  then Just (update2d x y 'X' (update2d (x + 1) y '>' maze), nextChar, if currentChar == 'X' then numberOfObstacles + 1 else numberOfObstacles)
                  else Just (update2d x y 'v' maze, currentChar, numberOfObstacles)

walk :: (V.Vector (V.Vector Char), Bool) -> Maybe (V.Vector (V.Vector Char), Bool)
walk (maze, done) =
  if done
    then (maze, done)
    else walk (applyStep maze)

obstructions :: ([String], Char, Int) -> Int
obstructions (maze, currentChar, currentNumberOfObstacles) =
  if not (any (anyCharInString guardSymbols) maze)
    then currentNumberOfObstacles
    else maybe currentNumberOfObstacles obstructions (applyStepWithObstructions (maze, currentChar, currentNumberOfObstacles))

numberOfPositions :: String -> Int
numberOfPositions =
  lines
    .> V.fromList
    .> V.map V.fromString
    .> walk
    .> intercalate ""
    .> filter (== 'X')
    .> length

numberOfObstructions :: String -> Int
numberOfObstructions =
  lines
    .> ( \maze -> obstructions (maze, '.', -1)
       )
