{-# LANGUAGE FlexibleContexts #-}

-- Thoughts and considerations
-- Previously:
-- - We worked only on strings - Str a, which is a constructor of Inline
-- - Substitution output was also a string
--
-- Now:
-- - We still will work only on plain strings - in most cases Str a (again an Inline)
-- - Metadata value (something to substitute) is itself a list of Inlines
-- - Substitution works different! We replace a single Inline with a list of inlines:
--     a) preceeding string literal, if any
--     b) list of Inlines we have for this key (variable)
--     c) succeeding string literal, if any
--
-- Major difference - tree structure didn't change previosly, now we introduce new elements

module MetaSubstitute (metaSubst) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc.Definition
import Text.Pandoc.Walk

import qualified Data.Map as M

import Text.YamlVars.Processor(parseStr, buildInlines, MetaDictionary)
import Text.YamlVars.Transform(transform, substMetaStr, substMetaBlock)

-- Tests (on Str)

testInlSubst :: String -> [Inline] -> TestTree
testInlSubst name value =
  testGroup name
  [ testCase "nothing" $ doSubst (Str "text")  @?= [Str "text"]
  , testCase "single"  $ doSubst (Str "%x%")   @?= value
  , testCase "prefix"  $ doSubst (Str "n-%x%") @?= [Str "n-"] ++ value
  , testCase "postfix" $ doSubst (Str "%x%th") @?= value ++ [Str "th"]
  ]
 where doSubst i = substMetaStr md i
       md        = M.fromList [("x", value)]

singleInline :: TestTree
singleInline = testInlSubst "Single inline" [Str "value!"]

multipleInlines :: TestTree
multipleInlines = testInlSubst "Multiple inlines" value
  where value = [ Str "haskell", Space, Str "is", Space, Strong [Str "fun"] ]

-- Tests (on document subtree)

testTreeSubst :: String -> ([Inline] -> Block) -> [Inline] -> TestTree
testTreeSubst name f value =
  testGroup name
  [ testCase "nothing" $ doSubst (f [Str "text" ]) @?= (f $ [Str "text"]       )
  , testCase "single"  $ doSubst (f [Str "%x%"  ]) @?= (f $ value              )
  , testCase "prefix"  $ doSubst (f [Str "n-%x%"]) @?= (f $ [Str "n-"] ++ value)
  , testCase "postfix" $ doSubst (f [Str "%x%th"]) @?= (f $ value ++ [Str "th"])
  ]
 where doSubst t = walk (substMetaBlock md) t
       md        = M.fromList [("x", value)]

treeTests :: String -> ([Inline] -> Block) -> TestTree
treeTests s f = testGroup s
                [ testTreeSubst "Single inline"    f [Str "value!"]
                , testTreeSubst "Multiple inlines" f value
                ]
  where value = [ Str "haskell", Space, Str "is", Space, Strong [Str "fun"] ]


metaSubst :: TestTree
metaSubst = testGroup "MetaInline substitutions"
            [ singleInline
            , multipleInlines
            , treeTests "Paragraph" Para
            , treeTests "Paragraph + strong" (\ii -> Para [Strong ii])
            , treeTests "Paragraph + header" (\ii -> Header 1 nullAttr ii)
            , treeTests "Paragraph + blocks" (\ii -> Para $ [Strong ii, Emph ii] ++ ii)
            ]
