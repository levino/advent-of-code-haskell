{-# LANGUAGE LambdaCase #-}

module TwentyTwentyFour.DaySix (numberOfPositions) where

import Control.Lens (Ixed (ix), element, (.~))
import Data.List
import Data.Maybe (isJust)
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

guardPosition :: [String] -> Maybe (Int, Int)
guardPosition =
  zip [0 ..]
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
                          then Just (x, y)
                          else Nothing
                )
                Nothing
                (zip [0 ..] line)
      )
      Nothing

guardDirection :: [String] -> Maybe Direction
guardDirection maze = guardPosition maze >>= (\(x, y) -> charToDirection (get2d x y maze))

applyStep :: [String] -> Maybe [String]
applyStep maze =
  guardPosition maze >>= \(x, y) ->
    guardDirection maze >>= \case
      UP ->
        if y == 0
          then Just (update2d x y 'X' maze)
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
          then Just (update2d x y 'X' maze)
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

step :: ([String], Int) -> ([String], Int)
step (maze, steps) =
  if not (any (anyCharInString guardSymbols) maze)
    then (maze, steps)
    else case applyStep maze of
      Just nextMaze -> step (nextMaze, steps + 1)
      Nothing -> (maze, steps)

numberOfPositions :: String -> Int
numberOfPositions =
  lines
    .> (\maze -> step (maze, 0))
    .> fst
    .> intercalate ""
    .> filter (== 'X')
    .> length
