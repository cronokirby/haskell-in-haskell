import qualified LexerTest
import Ourlude
import qualified ParserTest
import qualified SimplifierTest
import Test.Tasty
import qualified TyperTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ LexerTest.tests,
      ParserTest.tests,
      SimplifierTest.tests,
      TyperTest.tests
    ]
