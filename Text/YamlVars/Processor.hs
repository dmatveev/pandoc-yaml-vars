module Text.YamlVars.Processor
       ( process
       , parseStr
       , parseEntry
       , parseDict
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

parseVarName :: Parser String
parseVarName = many1 (letter <|> digit <|> char '-')

parseVar :: Parser ParsedElement
parseVar = PVar <$> (char '%' *> parseVarName <* char '%')

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

process :: String -> Dictionary -> String
process s d =
  case parseStr s of
   (Left _)   -> s
   (Right ps) -> buildStr ps d

parseDict :: Parser Dictionary
parseDict = mkDict <$> (optional (many newline) *> many parseEntry)

parseEntry :: Parser (String, String)
parseEntry = do
  var <- parseVarName
  many space
  char ':'
  many space
  str <- many1 (noneOf "\n")
  eof <|> (newline >> return ())
  return $ (var, str)
