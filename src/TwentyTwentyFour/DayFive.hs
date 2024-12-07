module TwentyTwentyFour.DayFive (sumValidMiddleValues) where

import Data.List (find)
import Data.List.Split
import Flow

parseInt :: String -> Int
parseInt = read

parseRules :: String -> [[Int]]
parseRules = lines .> map (splitOn "|" .> map parseInt)

parseUpdates :: String -> [[Int]]
parseUpdates = lines .> map (splitOn " " .> map parseInt)

sumValidMiddleValues :: String -> String -> Int
sumValidMiddleValues rulesString updatesString =
  let rules = parseRules rulesString
      updates = parseUpdates updatesString
   in updates
        |> filter
          ( foldl
                ( \oldValue currentNumber ->
                    let numbersAfter = rules |> filter (\rule -> rule !! 0 == currentNumber) |> map (!! 0)
                        numbersBefore = rules |> filter (\rule -> rule !! 1 == currentNumber) |> map (!! 1)
                     in True
                )
                0
          )
        |> foldl
          ( ( \sum
               update ->
                  sum
                    + update
                      !! (length update + 1 / 2)
            )
              0
          )