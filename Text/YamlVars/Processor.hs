module Text.YamlVars.Processor
       ( parseStr
       , buildStr
       , mkDict
       , ParsedString
       , ParsedElement(..)
       , Dictionary
       ) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Strict as M

type ParsedString = [ParsedElement]

data ParsedElement = PStr !String
                   | PVar !String
                     deriving (Eq, Show)

type Dictionary = M.Map String String

stringParser :: Parser ParsedString
stringParser = many (try parseVar <|> parseTxt)

parseVar :: Parser ParsedElement
parseVar = PVar <$> (char '%' *> manyTill anyChar (char '%'))

parseTxt :: Parser ParsedElement
parseTxt = PStr <$> do
  firstChar <- optionMaybe (char '%')
  remaining <- many1 (noneOf "%")
  return $ case firstChar of
   Nothing -> remaining
   Just x  -> x:remaining

parseStr :: String -> Either String ParsedString
parseStr s = case parse stringParser "" s of
              (Left e)   -> Left  $ show e
              (Right ps) -> Right $ ps


mkDict :: [(String, String)] -> Dictionary
mkDict = M.fromList

buildStr :: ParsedString -> Dictionary -> String
buildStr = undefined
