import Data.List.Split
import System.Environment
import Data.List (elemIndex)
import Data.Bifunctor
import Control.Monad.Trans.State.Strict (State, evalState, get)
import Debug.Trace (traceShow)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print . sum . map (read . snd) . filter (evalState isMadeOfRepeating) . map (([],) . show) $ concatMap (tupleToRange . stringToIntTuple . splitToTuple '-') . splitOn "," $ input

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
areHalfsEqual num = uncurry (==) . splitAt len $ numStr
    where
      numStr = show num
      len = length numStr `div` 2

isMadeOfRepeating :: State (String,String) Bool
isMadeOfRepeating =
  do
    (front,back) <- get
    case back of
      [] -> return False
      b:bs -> case doesRepeat (front++[b]) (front++b:bs) of
        True -> return True
        False -> return . evalState isMadeOfRepeating $ (front++[b],bs)

doesRepeat :: String -> String -> Bool
doesRepeat rep list = all (\a -> (r rep== r a) && (length a /= length list)) chunks
  where
    size = length rep
    chunks = splitListToChunks size list
    r = read :: String -> Int

splitListToChunks :: Int -> String -> [String]
splitListToChunks _ [] = []
splitListToChunks a b = take a b : splitListToChunks a (drop a b)
