import Test.Tasty
import TwentyTwentyFour.Test04 (tests04)
import TwentyTwentyFour.TestDayFive (tests05)

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [tests04, tests05]
