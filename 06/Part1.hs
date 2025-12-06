import Data.List

data Operation = Addition | Multiplication

main :: IO ()
main = getContents >>= print . sum . map carryOutOperation . parseListOfProblems . map words . lines

parseListOfProblems :: [[String]] -> [(Operation,[Int])]
parseListOfProblems = foldl parseProblem [] . transpose

parseProblem :: [(Operation,[Int])] -> [String] -> [(Operation,[Int])]
parseProblem parsedLs el = parsedLs ++ [(parseOperation . last $ el, map read. init $ el)]

parseOperation :: String -> Operation
parseOperation op =
  case op of
    "+" -> Addition
    "*" -> Multiplication

carryOutOperation :: (Operation,[Int]) -> Int
carryOutOperation (Addition,numLs) = sum numLs
carryOutOperation (Multiplication,numLs) = product numLs
