import Test.Tasty

import Processor

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "pandoc-yaml-vars tests" [ processor ]
