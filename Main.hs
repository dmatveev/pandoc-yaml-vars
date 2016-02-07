module Main where

import Data.Maybe (fromJust)
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.JSON
import Text.Parsec.String
import Text.YamlVars.Processor
import System.Exit (die)
import System.Environment (lookupEnv)

substBlock :: Dictionary -> Block -> Block
substBlock d b = case b of
  (CodeBlock a s) -> CodeBlock a (process s d)
  (RawBlock f s)  -> RawBlock f (process s d)
  otherwise       -> walk (substInl d) b

substInl :: Dictionary -> Inline -> Inline
substInl d i = case i of
  (Str s)            -> Str (process s d)
  (Code a s)         -> Code a (process s d)
  (Math m s)         -> Math m (process s d)
  (RawInline f s)    -> RawInline f (process s d)
  (Link ii (u,t))    -> Link  (p' ii) (process u d, process t d)
  (Image ii (u,t))   -> Image (p' ii) (process u d, process t d)
  otherwise          -> i
 where p' = walk (substInl d)

main :: IO ()
main = do
  mDictFile <- lookupEnv "VARSFILE"
  case mDictFile of
   Nothing   -> die "Please specify dictionary path in VARSFILE"
   (Just df) -> do
     md <- parseFromFile parseDict df
     case md of
      (Left e)  -> die $ show e
      (Right d) -> toJSONFilter (substBlock d)
