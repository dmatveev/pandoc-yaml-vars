module Text.YamlVars.Processor
       ( parseStr
       , buildStr
       , mkDict
       , ParsedString
       , ParsedElement(..)
       , Dictionary
       ) where

import Text.Parsec (parse)
import qualified Data.Map.Strict as M

type ParsedString = [ParsedElement]

data ParsedElement = PStr !String
                   | PVar !String
                     deriving (Eq, Show)

type Dictionary = M.Map String String

stringParser = undefined

parseStr :: String -> Either String ParsedString
parseStr s = case parse stringParser "" s of
              (Left e)   -> Left  $ show e
              (Right ps) -> Right $ ps


mkDict :: [(String, String)] -> Dictionary
mkDict = M.fromList

buildStr :: ParsedString -> Dictionary -> String
buildStr = undefined
