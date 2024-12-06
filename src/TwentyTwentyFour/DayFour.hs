module TwentyTwentyFour.DayFour (countXmas) where 

import Data.Function ((&))


countXmas :: String -> Int
countXmas input = input & lines & (\rows -> 
    lines & zip [1..] & 
    foldl (\numberOfXmasInLastRow (rowNumber, line) -> 
        line & zip [1..] & foldl (\numberOfXmasInCurrentRow (columnNumber, char) -> 0) 0 )
        0  
        )
     