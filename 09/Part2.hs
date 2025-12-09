import Data.List
import Data.Tuple
import Debug.Trace

type Point = (Int,Int)
type Line = (Point,Int,Point)

main :: IO ()
main = do
  input <- getContents
  let
    points = map ((\(a:b:_) -> (a,b)) . map readToInt . split ',') . lines $ input
    everyLines = everyLine points
    rectangles = everyRectangle points everyLines
  print rectangles

readToInt :: String -> Int
readToInt = read

everyRectangle :: [Point] -> [Line] -> [Int]
everyRectangle _ [] = []
everyRectangle points (l:ls) = map (unwrapPlotSafe . getRectangleIfSamePoint l) ls ++ everyRectangle points ls
  where
    getSharedPoint (pa1,_,pb1) (pa2,_,pb2)
      | pa1 == pa2 = Just pa1
      | pa1 == pb2 = Just pa1
      | pb1 == pa2 = Just pb1
      | pb1 == pb2 = Just pb1
      | otherwise = Nothing

    getNonSharedPoints sharedP (pa1,_,pb1) (pa2,_,pb2) = (if pa1 == sharedP then pb1 else pa1, if pa2 == sharedP then pb2 else pa2)
    getVector1To2 (x1,y1) (x2,y2) = (x2 - x1, y2 - y1)
    addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

    getRectangleIfSamePoint :: Line -> Line -> Maybe Int
    getRectangleIfSamePoint l1@(pa1,ln1,pb1) l2@(pa2,ln2,pb2) = traceShow ("Lines: " ++ show l1 ++ show l2) $ 
      case traceShowId . traceShow "Get rectangle" $ getSharedPoint l1 l2 of
        Nothing -> Nothing
        Just sharedPoint -> if opp `elem` points
            then Just . traceShowId $ (ln1 * ln2)
            else Nothing
          where
            (pa, pb) = traceShowId $ getNonSharedPoints sharedPoint l1 l2
            vec1 = traceShowId $ getVector1To2 sharedPoint pa
            vec2 = traceShowId $ getVector1To2 sharedPoint pb
            opp = traceShowId $ addVectors vec1 . addVectors vec2 $ sharedPoint
    -- getRectangleIfSamePoint = error "kfjskfjdsk"

    unwrapPlotSafe :: Maybe Int -> Int
    unwrapPlotSafe plot =
      case plot of
        Nothing -> -1
        Just x -> x

everyLine :: [Point] -> [Line]
everyLine [] = []
everyLine (p:ps) = (map (pointToLine p) . filter (arePointsInSameLine p) $ ps) ++ everyLine ps
  where
    pointToLine p1@(x1,y1) p2@(x2,y2) = (p1, if x1==x2 then abs (y1 - y2) + 1 else abs (x1 - x2) + 1, p2)
    arePointsInSameLine (x1,y1) (x2,y2)
        | x1 == x2 = True
        | y1 == y2 = True
        | otherwise = False

-- everyRectangle :: [Point] -> [Int]
-- everyRectangle [] = []
-- everyRectangle (p:ps) = map (getRectangle p) ps ++ everyRectangle ps
--
-- getRectangle :: Point -> Point -> Int
-- getRectangle (x1,y1) (x2,y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

split :: (Eq a) => a -> [a] -> [[a]]
split el eLs = split' el (eLs, [], [])

split' :: (Eq a) => a -> ([a], [a], [[a]]) -> [[a]]
split' el ([], [], resultLs) = resultLs
split' el ([], currLs, resultLs) = resultLs ++ [currLs]
split' el (e : eRest, currLs, resultLs) =
  if el == e
    then split' el (eRest, [], resultLs ++ [currLs])
    else split' el (eRest, currLs ++ [e], resultLs)
