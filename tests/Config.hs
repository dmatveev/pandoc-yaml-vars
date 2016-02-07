module Config (config) where

import Test.Tasty

import Text.YamlVars.Processor

import Common

testEntry :: String -> (String, String) -> TestTree
testEntry = testParser parseEntry

testFile :: String -> Dictionary -> TestTree
testFile = testParser parseDict

entries :: TestTree
entries = testGroup "Entry parser"
          [ testEntry "foo:bar"              ("foo", "bar")
          , testEntry "foo: bar"             ("foo", "bar")
          , testEntry "foo : bar"            ("foo", "bar")
          , testEntry "foo : bar baz"        ("foo", "bar baz")
          , testEntry "123 : 32 1"           ("123", "32 1")
          , testEntry "1-a : 22xyz"          ("1-a", "22xyz")
          , testEntry "l33 : t\n"            ("l33", "t")
          , testEntry "lng : hs\n\n\n"       ("lng", "hs")
          ]

file :: TestTree
file = testGroup "File parser"
       [ testFile "foo:bar\nbar:foo"         (mkDict [("foo","bar"), ("bar","foo")])
       , testFile "foo:bar\nbar:foo\n"       (mkDict [("foo","bar"), ("bar","foo")])
       , testFile "foo:bar\nbar:foo\n\n"     (mkDict [("foo","bar"), ("bar","foo")])
       , testFile "\nfoo:bar\nbar:foo\n\n"   (mkDict [("foo","bar"), ("bar","foo")])
       , testFile "\n\nfoo:bar\nbar:foo\n\n" (mkDict [("foo","bar"), ("bar","foo")])
       ]

config :: TestTree
config = testGroup "Configuration" [entries, file]
