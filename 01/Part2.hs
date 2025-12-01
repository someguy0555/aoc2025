import Debug.Trace (traceShow)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print . dialAtZero 50 . lines $ input

dialAtZero :: Int -> [String] -> Int
dialAtZero init ln = sum $ traceShow getAllDialsAtZero getAllDialsAtZero
  where
    getRotations = map getRotation ln
    getRawPositions = foldl (\a b -> a ++ [last a + b]) [init] getRotations
    getPositions = map (`mod` 100) getRawPositions
    zippedPos = traceShow (zip getPositions getRotations) zip getPositions getRotations
    getAllDialsAtZero = map func zippedPos
    func (a, b)
        | a == 0 = q
        | c < 0 || 100 < c = q + 1
        | c == 0 || c == 100 = q + 1
        | otherwise = q
      where
          r = b `rem` 100
          modl a = if a < 0 then a * (-1) else a
          q = modl (b `quot` 100)
          c = a + r

getRotation :: String -> Int
getRotation str = (getDirection . head $ str) * read (tail str)
  where
    getDirection c =
      if c == 'L'
        then (-1)
        else 1
