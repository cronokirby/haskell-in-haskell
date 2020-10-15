import qualified LexerTest
import Ourlude
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [LexerTest.tests]
