module TNParser 
	(parseTNGraphEdgesFile
	) where

import Text.ParserCombinators.Parsec
import TNTypes
import System.IO
import qualified Data.ByteString as S
import TNGraph as TNGraph
import qualified Data.Map.Strict as M

-- |Parses file contents into a TNGraph
-- Look at test_picture.txt to see file format
parseTNGraphEdgesFile :: String -> Either String TNGraph
parseTNGraphEdgesFile input = 
	let result = parse parseGraphEdgesFile "(unknown)" input
	in finalResult(result)

finalResult:: Either ParseError TGraphInfo -> Either String TNGraph
finalResult input = either (Left . show) (Right . (TNGraph.buildTNGraphFromInfo TNDGraph)) input

parseGraphEdgesFile = do
	skipMany comments
	edges <- endBy edgeLine eol
	skipMany comments
	vertexValues <- endBy vertexValueLine eol
	eof
	return (edges, M.fromList vertexValues)

vertexValueLine = do
	vertexId <- many1 digit
	tab
	sign <- optionMaybe ((char '+') <|> (char '-'))
	value <- many1 digit
	let vertexIdInt = read vertexId :: TNVertex
	let valueInt = read value :: TNVertexValue
	let signedInt = 
		case sign of
			Nothing -> valueInt
			Just(sign) -> if (sign == '-') then -valueInt else valueInt
	return (vertexIdInt, signedInt)

comments = do 
    string "#"
    comment <- (manyTill anyChar newline)
    return ""

edgeLine = do
	from <- many1 digit
	tab
	to <- many1 digit
	let fromInt = read from :: TNVertex
	let toInt = read to :: TNVertex
	return (fromInt, toInt)

eol = char '\n'
