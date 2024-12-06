import Test.Tasty
import TwentyTwentyFour.Test04 (tests04)

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ tests04
  ]
