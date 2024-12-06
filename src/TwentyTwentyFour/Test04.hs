module TwentyTwentyFour.Test04 (tests04) where

import Test.Tasty
import Test.Tasty.HUnit
import TwentyTwentyFour.DayFour (countXmas)

tests04 :: TestTree
tests04 = testGroup "Tests for 04"
  [ testCase "countXmas" $ countXmas "hallo" @?= 5
  ]
