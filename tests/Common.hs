module Common
       (
         testParser
       ) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec
import Text.Parsec.String

escape :: String -> String
escape []        = []
escape ('\n':xs) = '\\':'n':escape xs
escape ('\t':xs) = '\\':'t':escape xs
escape (x:xs)    = x:escape xs

testParser :: (Eq a, Show a) => Parser a -> String -> a -> TestTree
testParser p s a =
  testCase (escape s) $
     case (parse p "" s) of
      (Left e)   -> error $ show e
      (Right a') -> a' @?= a

