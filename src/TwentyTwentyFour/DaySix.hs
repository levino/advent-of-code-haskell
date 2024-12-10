module TwentyTwentyFour.DaySix (numberOfPositions) where

import Data.Maybe (isJust)
import Debug.Trace (traceShow)
import Flow

guardSymbols = ['>', '<', '^', 'v']

anyCharInString :: [Char] -> String -> Bool
anyCharInString chars str = any (`elem` str) chars

guardPosition :: [String] -> Maybe (Int, Int)
guardPosition =
  zip [0 ..]
    .> foldl
      ( \outerAcc (x, line) ->
          if isJust outerAcc
            then outerAcc
            else
              foldl
                ( \innerAcc (y, char) ->
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

guardDirection :: [String] -> Maybe Char
guardDirection zeMap =
  case guardPosition zeMap of
    Nothing -> Nothing
    Just (x, y) -> Just (zeMap !! x !! y)

applyStep :: ([String]) -> [String]
applyStep zeMap =
  guardPosition zeMap >>= \(x,y) ->
    guardDirection >>= \direction ->
      []

step :: ([String], Int) -> ([String], Int)
step (zeMap, steps) =
  if not (any (anyCharInString guardSymbols) zeMap)
    then (zeMap, steps)
    else step (zeMap, steps + 1)

numberOfPositions :: String -> Int
numberOfPositions _ =
  5
