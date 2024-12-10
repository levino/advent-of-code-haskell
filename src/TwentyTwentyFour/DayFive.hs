module TwentyTwentyFour.DayFive (sumValidMiddleValues, fixAndSumFixedMiddleValues) where

import Data.List (sortBy)
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

orderCorrect :: [[Int]] -> [Int] -> Bool
orderCorrect rules update =
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

sumMiddleValues :: [[Int]] -> Int
sumMiddleValues =
  foldl
    ( \currentSum
       update ->
          currentSum
            + update
              !! ((length update - 1) `div` 2)
    )
    0

sumValidMiddleValues :: String -> String -> Int
sumValidMiddleValues rulesString updatesString =
  let rules = parseRules rulesString
      updates = parseUpdates updatesString
   in updates
        |> filter (orderCorrect rules)
        |> sumMiddleValues

fixOrder :: [[Int]] -> [Int] -> [Int]
fixOrder rules =
  sortBy
    ( \a b ->
        if any (\element -> head element == a && element !! 1 == b) rules
          then LT
          else
            if any (\element -> head element == b && element !! 1 == a) rules
              then GT
              else EQ
    )

fixAndSumFixedMiddleValues :: String -> String -> Int
fixAndSumFixedMiddleValues rulesString updatesString =
  let rules =
        parseRules rulesString
          |> sortBy
            ( \a b ->
                if head a /= head b
                  then compare (head a) (head b)
                  else compare (b !! 1) (a !! 1)
            )

      updates = parseUpdates updatesString
   in updates
        |> filter (not . orderCorrect rules)
        |> map (fixOrder rules)
        |> sumMiddleValues