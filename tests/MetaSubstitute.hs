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
import qualified Data.Map as M

import Text.YamlVars.Processor (parseStr, buildInlines, MetaDictionary)

-- Temporary definitions (to be re|moved)
substMetaStr :: MetaDictionary -> Inline -> [Inline]
substMetaStr dict i@(Str s) = case parseStr s of
  (Left _)  -> [i]
  (Right ps)-> buildInlines ps dict
substMetaStr _    _         = error "substMetaStr works with strings only!"


-- Tests

testInlSubst :: String -> [Inline] -> TestTree
testInlSubst name value = testGroup name
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

metaSubst :: TestTree
metaSubst = testGroup "MetaInline substitutions" [ singleInline, multipleInlines ]
