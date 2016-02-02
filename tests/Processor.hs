module Processor (processor) where

import Test.Tasty
import Test.Tasty.HUnit


import Text.YamlVars.Processor

testParser :: String -> ParsedString -> TestTree
testParser s p = testCase s $ case parseStr s of
                               (Left e)   -> error e
                               (Right ps) -> ps @?= p

testBuilder :: ParsedString -> Dictionary -> String -> TestTree
testBuilder p d s = testCase s $ buildStr p d @?= s  

parser :: TestTree
parser = testGroup "Expression parser"
         [ testParser "%var%"              [PVar "var"]
         , testParser "file"               [PStr "file"]
         , testParser "%con%%cat%"         [PVar "con", PVar "cat"]
         , testParser "%con%%cat"          [PVar "con", PStr "%cat"]
         , testParser "%name%@example.com" [PVar "name",  PStr "@example.com"]
         , testParser "user@%domain%"      [PStr "user@", PVar "domain"]
         , testParser "foo-%bar%-baz"      [PStr "foo-", PVar "bar", PStr "-baz"]
         , testParser "%name%@%domain%"    [PVar "name", PStr "@", PVar "domain"]
         ]

builder :: TestTree
builder = testGroup "String builder"
          [ testBuilder [PVar "var"]                            d "some var value"
          , testBuilder [PStr "text"]                           d "text"
          , testBuilder [PVar "login", PStr "@", PVar "domain"] d "user@example.com"
          , testBuilder [PVar "foo", PVar "foo"]                d "fooValuefooValue"
          , testBuilder [PStr "variable", PVar "missing"]       d "variable%missing%"
          ]
  where d = mkDict [ ("var",    "some var value")
                   , ("login",  "user")
                   , ("domain", "example.com")
                   , ("foo",    "fooValue")
                   ]

processor :: TestTree
processor = testGroup "String processor" [parser, builder]
