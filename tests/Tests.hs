import Test.Tasty

import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "pandoc-yaml-vars tests" [ parser ]
