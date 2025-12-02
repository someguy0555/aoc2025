import Data.List.Split
import System.Environment
import Data.List (elemIndex)
import Data.Bifunctor

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print . sum . concatMap (filter areHalfsEqual . filter (even . length . show) . tupleToRange . stringToIntTuple . splitToTuple '-') . splitOn "," $ input

splitToTuple el list =
  case index of
    Just i -> second (drop 1) $ splitAt i list
    Nothing -> (list, [])
  where
    index = elemIndex el list

stringToIntTuple (a,b) = (read a, read b) :: (Int, Int)

tupleToRange :: (Int, Int) -> [Int]
tupleToRange (a,b) = [a..b]

areHalfsEqual :: Int -> Bool
areHalfsEqual a = uncurry (==) . splitAt len $ b
    where
      b = show a
      len = length b `div` 2
