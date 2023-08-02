{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Parser where


import Text.Parsec
import Functions
import Types
import Data.Functor (void)
import qualified Data.Map as Map
import Control.Monad (replicateM_)




type Parser = Parsec String ()


name :: Parser String
name = skipMany (char '{' <|> char '[') `between` skipMany (char '}' <|> char ']') $ many1 letter

csvPts :: Parser [String]
csvPts = name `sepBy1` string ", "

name' :: Parser String
name' = skipMany (char '{' <|> char '[' <|> char '(') `between` skipMany (char '}' <|> char ']' <|> char ')') $ many1 letter

csvPts' :: Parser [String]
csvPts' = name' `sepBy1` string ", "

getArgs :: Parser [String]
getArgs = string "(" `between` string ")" $ csvPts 


constructionParser :: Parser [Command]
constructionParser = do
    configuration <- name
    void $ string ": "
    points <- csvPts
    pure $ initials Map.! configuration $ points


commandParser :: Parser Command
commandParser = do
    newPoint <- name
    void $ string " = "
    command <- many1 letter
    args <- getArgs
    pure $ Command newPoint args $ functions Map.! command


conclusionParser :: Parser Conclusion
conclusionParser = do
    factName <- name
    void $ string ": "
    points <- csvPts'
    pure $ Conclusion points $ factCheckers Map.! factName

theoremParser :: Parser ([Command], Conclusion)
theoremParser = do
    -- skipMany1 $ char '-'
    -- void $ char '\n'
    -- void $ string "Theorem "
    -- skipMany1 digit
    -- void $ char '\n'
    -- skipMany1 $ char '-'
    -- void $ char '\n'
    -- void $ char '\n'

    construction <- constructionParser
    void $ char '\n'
    commands <- try commandParser `sepEndBy1` char '\n'
    -- void $ char '\n'
    conclusion <- conclusionParser
    replicateM_ 5 $ skipMany (satisfy (/= '\n')) >> char '\n'
    pure (construction <> commands, conclusion)

parseThrorem :: SourceName -> String -> Either ParseError ([Command], Conclusion)
parseThrorem = parse (theoremParser <* eof)
