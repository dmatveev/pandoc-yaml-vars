import Test.Tasty

import Processor
import Config
import MetaSubstitute

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "pandoc-yaml-vars tests" [ processor, config, metaSubst ]
