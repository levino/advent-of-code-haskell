import Test.Tasty

import TwentyTwentyFour.TestDaySix (tests06)

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [tests06]
