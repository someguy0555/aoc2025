import Data.List.Split
import System.Environment
import Data.List (elemIndex)
import Data.Bifunctor

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print . sum . map (findLargestVoltage 12 . map charToDigit) . lines $ input

charToDigit :: Char -> Int
charToDigit c = read [c]

upToMax :: [Int] -> (Int,[Int])
upToMax l = (maximum l,) . upToMax' $ l
  where
    upToMax' (ll:lls) =
      if ll == maximum l
          then lls
          else upToMax' lls

findLargestVoltage :: Int -> [Int] -> Int
findLargestVoltage = findLargestVoltage' 0

findLargestVoltage' :: Int -> Int -> [Int] -> Int
findLargestVoltage' _ _ [] = error "The list isn't supposed to be empty btw."
findLargestVoltage' acc n ls
  | length ls < n = error "list too small"
  | n < 1 = error "n < 1"
  | n == 1 = acc * 10 + f
  | otherwise = findLargestVoltage' (acc * 10 + f) (n - 1) (rest ++ laste)
  where
    start = take (length ls - n + 1) ls
    laste = reverse . take (n - 1) . reverse $ ls
    (f,rest) = upToMax start
