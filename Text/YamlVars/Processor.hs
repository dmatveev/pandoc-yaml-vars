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
parseVar = do
  _ <- char '%'
  var <- many1 (letter <|> digit <|> char '-')
  _ <- char '%'
  return $ PVar var

parseTxt :: Parser ParsedElement
parseTxt = PStr <$> (try startsP <|> noP)
  where startsP = (++) <$> pp <*> mnoP
        mnoP    = option "" (noP)
        noP     = many1 (noneOf "%")
        pp      = many1 $ char '%'


parseStr :: String -> Either String ParsedString
parseStr s = case parse stringParser "" s of
              (Left e)   -> Left  $ show e
              (Right ps) -> Right $ ps


mkDict :: [(String, String)] -> Dictionary
mkDict = M.fromList

buildStr :: ParsedString -> Dictionary -> String
buildStr p d = concat $ map (subst d) p
  where 
    subst _ (PStr s) = s
    subst _ (PVar v) = case M.lookup v d of
          Nothing  -> "%" ++ v ++ "%"
          (Just s) -> s
