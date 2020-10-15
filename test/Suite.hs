import Ourlude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "2 + 2 = 4" (2 + 2 @=? 4)
    ]
