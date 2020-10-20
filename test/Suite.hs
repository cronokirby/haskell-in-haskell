import qualified LexerTest
import Ourlude
import qualified ParserTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [LexerTest.tests, ParserTest.tests]
