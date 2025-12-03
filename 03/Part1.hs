import Data.List.Split
import System.Environment
import Data.List (elemIndex)
import Data.Bifunctor

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print . sum . map (findLargestVoltage . map charToDigit) . lines $ input

charToDigit :: Char -> Int
charToDigit c = read [c]

upToMax :: [Int] -> (Int,[Int])
upToMax l = (maximum l,) . upToMax' $ l
  where
    upToMax' (ll:lls) =
      if ll == maximum l
          then lls
          else upToMax' lls

findLargestVoltage :: [Int] -> Int
findLargestVoltage [] = error "The list isn't supposed to be empty btw."
findLargestVoltage l = f * 10 + b
  where
    start = init l
    laste = last l
    (f,rest) = upToMax start
    (b,_) = upToMax (rest ++ [laste])
