module Config (config) where

import Test.Tasty

import Text.YamlVars.Processor

import Common

testEntry :: String -> (String, String) -> TestTree
testEntry = testParser parseEntry

testFile :: String -> Dictionary -> TestTree
testFile = testParser parseFile

testDoc :: String -> [(String,String)] -> TestTree
testDoc = testParser parseDoc

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

sampleDoc :: String
sampleDoc = "\
\---\n\
\foo: bar\n\
\123: 456"

sampleDoc2 :: String
sampleDoc2 = "\
\foo: bar\n\
\123: 456"

sampleMultipleDocs :: String
sampleMultipleDocs = "\
\---\n\
\foo: bar\n\
\---\n\
\123: 456"

sampleDocWithEnding :: String
sampleDocWithEnding = "\
\---\n\
\foo: bar\n\
\123: 456\n\
\..."

sampleMultipleDocsWithEnding :: String
sampleMultipleDocsWithEnding = "\
\---\n\
\foo: bar\n\
\...\n\
\---\n\
\123: 456\n\
\..."

document :: TestTree
document = testGroup "Single document"
           [ testDoc sampleDoc                    [("foo","bar"), ("123", "456")]
           , testDoc sampleDoc2                   [("foo","bar"), ("123", "456")]
           , testDoc sampleDocWithEnding          [("foo","bar"), ("123", "456")]
           ]

structure :: TestTree
structure = testGroup "Document structure"
            [ testFile sampleDoc                    (mkDict [("foo","bar"), ("123", "456")])
            , testFile sampleDoc2                   (mkDict [("foo","bar"), ("123", "456")])
            , testFile sampleMultipleDocs           (mkDict [("foo","bar"), ("123", "456")])
            , testFile sampleDocWithEnding          (mkDict [("foo","bar"), ("123", "456")])
            , testFile sampleMultipleDocsWithEnding (mkDict [("foo","bar"), ("123", "456")])
            ]

config :: TestTree
config = testGroup "Configuration" [entries, file, document, structure]
