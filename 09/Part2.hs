import Data.List

type Point = (Int,Int)
type Rectangle = (Point,Int,Point)

main :: IO ()
main = do
  input <- getContents
  let
    points = map ((\(a:b:_) -> (a,b)) . map readToInt . split ',') . lines $ input
    rectangles = everyRectangle points
  print . sortBy (\(_,plot1,_) (_,plot2,_) -> compare plot2 plot1) $ rectangles

readToInt :: String -> Int
readToInt = read

everyRectangle :: [Point] -> [Rectangle]
everyRectangle [] = []
everyRectangle (p:ps) = map (getRectangle p) ps ++ everyRectangle ps

getRectangle :: Point -> Point -> Rectangle
getRectangle p1@(x1,y1) p2@(x2,y2) = (p1,(abs (x1 - x2) + 1) * (abs (y1 - y2) + 1),p2)

split :: (Eq a) => a -> [a] -> [[a]]
split el eLs = split' el (eLs, [], [])

split' :: (Eq a) => a -> ([a], [a], [[a]]) -> [[a]]
split' el ([], [], resultLs) = resultLs
split' el ([], currLs, resultLs) = resultLs ++ [currLs]
split' el (e : eRest, currLs, resultLs) =
  if el == e
    then split' el (eRest, [], resultLs ++ [currLs])
    else split' el (eRest, currLs ++ [e], resultLs)
