import Data.List.Split
import Data.Bifunctor
import Data.List

main :: IO ()
main = getContents >>= \input -> let
    freshIngredients = nub . fst $ parsedInput input
    ingredients = snd $ parsedInput input
    parsedInput = bimap stringToRangeTuple (map readToInt) . (\(x:y:z) -> (x,y)) . splitWhen null . lines
  in print . foldl (\acc (r1,r2) -> acc + (r2 - r1 + 1)) 0 . combineOverlappingRanges $ freshIngredients

combineOverlappingRanges :: (Num a, Ord a) => [(a,a)] -> [(a,a)]
combineOverlappingRanges [] = []
combineOverlappingRanges rangeLs = fst . foldl iterateFoldRanges ([head sortedLs],head sortedLs) . tail $ sortedLs
  where
    sortedLs = sortBy (\(a1,_) (b1,_) -> compare a1 b1) rangeLs
    iterateFoldRanges (combinedLs,lastEl) el =
        if doRangesOverlap lastEl el
        then (\newEl -> (init combinedLs ++ [newEl], newEl)) (combineRanges lastEl el)
        else (combinedLs ++ [el], el)

combineRanges :: Ord a => (a,a) -> (a,a) -> (a,a)
combineRanges (a1,a2) (_,b2) = (a1, max a2 b2)

doRangesOverlap :: Ord a => (a,a) -> (a,a) -> Bool
doRangesOverlap (_,a2) (b1,_) = b1 <= a2

isPartOfRanges :: Ord a => [(a,a)] -> a -> Bool
isPartOfRanges rangeList el = any (\(r1,r2) -> r1 <= el && el <= r2) rangeList

-- stringRangesToList = concatMap (tupleToRange . stringToIntTuple . splitToTuple '-')
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
