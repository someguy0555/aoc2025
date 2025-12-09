import Data.List

type Point = (Int,Int)

main :: IO ()
main = do
  input <- getContents
  print . maximum . everyRectangle . map ((\(a:b:_) -> (a,b)) . map readToInt . split ',') . lines $ input

readToInt :: String -> Int
readToInt = read

everyRectangle :: [Point] -> [Int]
everyRectangle [] = []
everyRectangle (p:ps) = map (getRectangle p) ps ++ everyRectangle ps

getRectangle :: Point -> Point -> Int
getRectangle (x1,y1) (x2,y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

split :: (Eq a) => a -> [a] -> [[a]]
split el eLs = split' el (eLs, [], [])

split' :: (Eq a) => a -> ([a], [a], [[a]]) -> [[a]]
split' el ([], [], resultLs) = resultLs
split' el ([], currLs, resultLs) = resultLs ++ [currLs]
split' el (e : eRest, currLs, resultLs) =
  if el == e
    then split' el (eRest, [], resultLs ++ [currLs])
    else split' el (eRest, currLs ++ [e], resultLs)
