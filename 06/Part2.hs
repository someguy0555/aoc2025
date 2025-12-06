import Data.List
import Data.Bifunctor

data Operation = Addition | Multiplication

main :: IO ()
main = getContents >>= print . sum . map carryOutOperation . parseListOfProblems . getColumns . getLinesAndColumnSizes . map getLineAndColumnSizes . lines

getColumns :: ([Int],[String]) -> [[String]]
getColumns (sizes,lines) = map parseLine lines
  where
    parseLine :: String -> [String]
    parseLine line = fst . foldl
      (\(acc,left) s ->
        (acc ++ [init . take s $ left]
        , drop s left)
      ) ([],line ++ " ") . map (+1) $ sizes

getLinesAndColumnSizes :: [(String,[Int])] -> ([Int],[String])
getLinesAndColumnSizes thing = (map maximum . transpose . map snd $ thing, map fst thing)

getLineAndColumnSizes :: String -> (String,[Int])
getLineAndColumnSizes line = (line, map length . words $ line)

parseListOfProblems :: [[String]] -> [(Operation,[Int])]
parseListOfProblems = foldl parseProblem [] . map (second transpose . \ls -> (last ls, init ls)) . transpose

parseProblem :: [(Operation,[Int])] -> (String,[String]) -> [(Operation,[Int])]
parseProblem parsedLs (strOp,strNumLs) = parsedLs ++ [(parseOperation strOp, map read strNumLs)]

parseOperation :: String -> Operation
parseOperation op =
  case parsedOp of
    '+' -> Addition
    '*' -> Multiplication
  where
    parsedOp = head . filter (/=' ') $ op

carryOutOperation :: (Operation,[Int]) -> Int
carryOutOperation (Addition,numLs) = sum numLs
carryOutOperation (Multiplication,numLs) = product numLs
