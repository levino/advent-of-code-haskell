module TwentyTwentyFour.DayFour (countXmas) where

import Data.Function ((&))

countXmas :: String -> Int
countXmas input =
  input
    & lines
    & ( \rows ->
          rows
            & zip [0 ..]
            & foldl
              ( \numberOfXmasInLastRow (rowNumber, line) ->
                  numberOfXmasInLastRow
                    + ( zip [0 ..] line
                          & foldl
                            ( \numberOfXmasInCurrentRow (columnNumber, char) ->
                                ( if rowNumber == 0 || columnNumber == 0 || rowNumber == length rows - 1 || columnNumber == length line - 1 || char /= 'A'
                                    then numberOfXmasInCurrentRow
                                    else
                                      let mas1 = [rows !! (rowNumber - 1) !! (columnNumber - 1), char, rows !! (rowNumber + 1) !! (columnNumber + 1)]
                                          mas2 = [rows !! (rowNumber - 1) !! (columnNumber + 1), char, rows !! (rowNumber + 1) !! (columnNumber - 1)]
                                          xMas = (mas1 == "MAS" || mas1 == "SAM") && (mas2 == "MAS" || mas2 == "SAM")
                                       in if xMas
                                            then numberOfXmasInCurrentRow + 1
                                            else numberOfXmasInCurrentRow
                                )
                            )
                            0
                      )
              )
              0
      )
