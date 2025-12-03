import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print . sum . map (findLargestVoltage . map charToDigit) . lines $ input

charToDigit :: Char -> Int
charToDigit c = read [c]

upToMax :: [Int] -> (Int,[Int])
upToMax l = (maximum l,) . upToMax' $ l
  where upToMax' = drop 1 . dropWhile (maximum l /=)

findLargestVoltage :: [Int] -> Int
findLargestVoltage [] = error "The list isn't supposed to be empty btw."
findLargestVoltage l = f * 10 + b
  where
    start = init l
    laste = last l
    (f,rest) = upToMax start
    (b,_) = upToMax (rest ++ [laste])
