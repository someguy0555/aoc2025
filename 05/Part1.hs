import Data.List.Split
import Data.Bifunctor
import Data.List

main :: IO ()
main = getContents >>= \input -> let
    freshIngredients = nub . fst $ parsedInput input
    ingredients = snd $ parsedInput input
    parsedInput = bimap stringToRangeTuple (map readToInt) . (\(x:y:z) -> (x,y)) . splitWhen null . lines
  in print . foldl (\x y -> if y then x + 1 else x) 0 . map (isPartOfRanges freshIngredients) $ ingredients

isPartOfRanges :: Ord a => [(a,a)] -> a -> Bool
isPartOfRanges rangeList el = any (\(r1,r2) -> r1 <= el && el <= r2) rangeList

stringToRangeTuple = map (stringToIntTuple . splitToTuple '-')

readToInt :: String -> Int
readToInt = read

splitToTuple el list =
  case index of
    Just i -> second (drop 1) $ splitAt i list
    Nothing -> (list, [])
  where
    index = elemIndex el list

stringToIntTuple (a,b) = (read a, read b) :: (Int, Int)

tupleToRange :: (Int, Int) -> [Int]
tupleToRange (a,b) = [a..b]
