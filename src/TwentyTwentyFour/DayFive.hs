module TwentyTwentyFour.DayFive (sumValidMiddleValues) where

import Data.List.Split
import Flow

parseInt :: String -> Int
parseInt = read

parseRules :: String -> [[Int]]
parseRules = lines .> map (splitOn "|" .> map parseInt)

parseUpdates :: String -> [[Int]]
parseUpdates =
  lines
    .> map
      ( splitOn ","
          .> map
            parseInt
      )

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start) (drop start xs)

anyIn :: [Int] -> [Int] -> Bool
anyIn values list =
  foldl
    ( \acc value ->
        if acc
          then acc
          else value `elem` list
    )
    False
    values

sumValidMiddleValues :: String -> String -> Int
sumValidMiddleValues rulesString updatesString =
  let rules = parseRules rulesString
      updates = parseUpdates updatesString
   in updates
        |> filter
          ( \update ->
              zip [0 ..] update
                |> foldl
                  ( \oldValue (index, currentNumber) ->
                      if not oldValue
                        then oldValue
                        else
                          let forbiddenNumbersBefore = rules |> filter (\rule -> head rule == currentNumber) |> map (!! 1)
                              forbiddenNumbersAfter = rules |> filter (\rule -> rule !! 1 == currentNumber) |> map (!! 0)
                              numbersBefore = slice 0 index update
                              numbersAfter = slice index (length update - 1) update
                           in not (anyIn forbiddenNumbersBefore numbersBefore || anyIn forbiddenNumbersAfter numbersAfter)
                  )
                  True
          )
        |> foldl
          ( \currentSum
             update ->
                currentSum
                  + update
                    !! ((length update - 1) `div` 2)
          )
          0
