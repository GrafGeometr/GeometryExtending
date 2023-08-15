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

goodSymbol :: Parser Char
goodSymbol = letter <|> digit

name :: Parser String
name = skipMany (char '{' <|> char '[') `between` skipMany (char '}' <|> char ']') $ many1 goodSymbol

csvPts :: Parser [String]
csvPts = name `sepBy1` string ", "

name' :: Parser String
name' = skipMany (char '{' <|> char '[' <|> char '(') `between` skipMany (char '}' <|> char ']' <|> char ')') $ many1 goodSymbol

csvPts' :: Parser [String]
csvPts' = name' `sepBy1` string ", "

name'' :: Parser String
name'' = many1 goodSymbol

csvPts'' :: Parser [String]
csvPts'' = name'' `sepBy1` string ", "

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

data ConclusionParseObj = Obj String | MkLine [String] | MkCircle [String] deriving Show

toArgs :: ConclusionParseObj -> [String]
toArgs (Obj s) = [s]
toArgs (MkLine s) = s
toArgs (MkCircle s) = s

conclObj :: Parser ConclusionParseObj
conclObj = do
    objName <- many1 goodSymbol
    pure $ Obj objName

conclLine :: Parser ConclusionParseObj
conclLine = do
    void $ char '['
    points <- csvPts''
    void $ char ']'
    pure $ MkLine points

conclCircle :: Parser ConclusionParseObj
conclCircle = do
    void $ char '('
    points <- csvPts''
    void $ char ')'
    pure $ MkCircle points

convertFromSimple :: [ConclusionParseObj] -> [Shape] -> [Shape]
convertFromSimple [] _ = []
convertFromSimple (Obj _ : xs) (shp:shps) = shp : convertFromSimple xs shps
convertFromSimple (MkLine _ : xs) ((PointShape p1):(PointShape p2):shps) = LineShape (line p1 p2) : convertFromSimple xs shps
convertFromSimple (MkCircle _ : xs) ((PointShape p1):(PointShape p2):(PointShape p3):shps) = CircleShape (circumcircle p1 p2 p3) : convertFromSimple xs shps

conclusionParser :: Parser Conclusion
conclusionParser = do
    factName <- name
    void $ string ": "
    objects <- (conclObj <|> conclLine <|> conclCircle) `sepBy1` string ", "

    let points = concat $ toArgs <$> objects
    pure $ Conclusion points $ (factCheckers Map.! factName) . (convertFromSimple objects)

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

parseTheorem :: SourceName -> String -> Either ParseError ([Command], Conclusion)
parseTheorem = parse (theoremParser <* eof)
