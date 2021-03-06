module Text.YamlVars.Processor
       ( process
       , parseStr
       , parseDoc
       , parseEntry
       , parseFile
       , buildStr
       , buildInlines
       , mkDict
       , ParsedString
       , ParsedElement(..)
       , Dictionary
       , MetaDictionary
       ) where

import Text.Parsec
import Text.Parsec.String
import Text.Pandoc.Definition (Inline(..))
import qualified Data.Map.Strict as M

type ParsedString = [ParsedElement]

data ParsedElement = PStr !String
                   | PVar !String
                     deriving (Eq, Show)

type Dictionary = M.Map String String
type MetaDictionary = M.Map String [Inline]

stringParser :: Parser ParsedString
stringParser = many (try parseVar <|> parseTxt)

parseVarName :: Parser String
parseVarName = do
  firstChar <- validChar
  following <- many (validChar <|> char '-')
  return $ firstChar : following
 where validChar = letter <|> digit

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
buildStr p d = concat $ map subst p
  where
    subst (PStr s) = s
    subst (PVar v) = case M.lookup v d of
      Nothing  -> "%" ++ v ++ "%"
      (Just s) -> s

buildInlines :: ParsedString -> MetaDictionary -> [Inline]
buildInlines p d = concat $ map subst p
  where
    subst (PStr s) = [Str s]
    subst (PVar v) = case M.lookup v d of
      Nothing   -> [Str ("%" ++ v ++ "%")]
      (Just ii) -> ii

process :: String -> Dictionary -> String
process s d =
  case parseStr s of
   (Left _)   -> s
   (Right ps) -> buildStr ps d

parseFile :: Parser Dictionary
parseFile = mkDict . concat <$> many1 parseDoc

parseEntry :: Parser (String, String)
parseEntry = do
  var <- parseVarName
  many space
  char ':'
  many space
  str <- many1 (noneOf "\n")
  eof <|> (newline >> return ())
  return $ (var, str)

parseDocStart :: Parser ()
parseDocStart = string "---" >> return ()

parseDocEnd :: Parser ()
parseDocEnd = string "..." >> return ()

whitespace :: Parser ()
whitespace = many (space <|> newline) >> return ()

parseDoc :: Parser [(String, String)]
parseDoc = do
  whitespace
  optional parseDocStart
  whitespace
  entries <- many1 parseEntry
  whitespace
  optional parseDocEnd
  whitespace
  return entries
