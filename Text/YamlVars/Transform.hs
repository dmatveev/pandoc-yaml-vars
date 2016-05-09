module Text.YamlVars.Transform
       (
         transform
       , substMetaStr
       , substMetaInl
       , substMetaBlock
       ) where

import Text.YamlVars.Processor (parseStr, buildInlines, MetaDictionary)
import Text.Pandoc.Walk
import Text.Pandoc.Definition
import qualified Data.Map.Strict as M

substMetaStr :: MetaDictionary -> Inline -> [Inline]
substMetaStr dict i@(Str s) = do
  case parseStr s of
    (Left _)  -> [i]
    (Right ps)-> buildInlines ps dict
substMetaStr _    i = [i]

substMetaInl :: MetaDictionary -> Inline -> Inline
substMetaInl dict i = case i of
  (Emph ii)         -> Emph        $ process ii
  (Strong ii)       -> Strong      $ process ii
  (Strikeout ii)    -> Strikeout   $ process ii
  (Superscript ii)  -> Superscript $ process ii
  (Subscript ii)    -> Subscript   $ process ii
  (SmallCaps ii)    -> SmallCaps   $ process ii
  (Quoted q ii)     -> Quoted q    $ process ii
  (Cite cc ii)      -> Cite cc     $ process ii
  (Link attr ii t)  -> Link  attr  ( process ii ) t
  (Image attr ii t) -> Image attr  ( process ii ) t
  (Span attr ii)    -> Span  attr  ( process ii )
  (Note bb)         -> Note        $ map (walk (substMetaBlock dict)) bb
  _                 -> i
 where
   process ii = map (walk (substMetaInl dict)) $ concat $ map (substMetaStr dict) ii

substMetaBlock :: MetaDictionary -> Block -> Block
substMetaBlock dict b = case b of
  (Plain ii)             -> Plain           $ process ii
  (Para ii)              -> Para            $ process ii
  (DefinitionList dl)    -> DefinitionList  $ map processDef dl
  (Header lvl attr ii)   -> Header lvl attr $ process ii
  (Table ii aa dd hh cc) -> Table (process ii) aa dd (processBB hh) (map processBB cc)
  _                      -> walk (substMetaInl dict) b
 where
   process ii         = map (walk (substMetaInl dict)) $ concat $ map (substMetaStr dict) ii
   processBB          = map (walk (substMetaBlock dict))
   processDef (ii,bb) = (process ii, processBB bb)


transform :: Maybe Format -> Pandoc -> Pandoc
transform mf (Pandoc meta blocks) = Pandoc meta $ walk (substMetaBlock md) blocks
  where md = M.fromList $ map toInlines $ filter isMetaInlines $ M.toList $ unMeta meta
        isMetaInlines (_, MetaInlines _)  = True
        isMetaInlines _                   = False
        toInlines     (k, MetaInlines ii) = (k, ii)
        toInlines     _                   = error "should never happen!"
