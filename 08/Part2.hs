import Data.List

type Point = (Int, Int, Int)

type Graph = [(Point, Float, Point)]

type Circuit = [Point]

main :: IO ()
main = do
  input <- getContents
  let points = map ((\(a : b : c : _) -> (a, b, c)) . map readToInt . split ',') . lines $ input
      graph = sortBy (\(_, d1, _) (_, d2, _) -> compare d1 d2) . constructGraph $ points
  print . multiplyPoints . unwrapMaybe $ getCircuits points graph
  -- print $ graph

multiplyPoints ((x1,_,_),(x2,_,_)) = x1 * x2

getCircuits :: [Point] -> Graph -> Maybe (Point,Point)
getCircuits points graph = getCircuits' graph $ map (: []) points

getCircuits' :: Graph -> [Circuit] -> Maybe (Point,Point)
getCircuits' [] currCircuits = Nothing
getCircuits' ((p1, _, p2) : connLeft) currCircuits = if length connected == 1 then Just (p1,p2) else getCircuits' connLeft connected
  where
    connected = connectCircuits p1 p2 currCircuits

connectCircuits :: Point -> Point -> [Circuit] -> [Circuit]
connectCircuits p1 p2 circuits =
  if circuit1 == circuit2
      then circuits
      else (circuit1 ++ circuit2) : (delete circuit1 . delete circuit2 $ circuits)
  where
    isPointInCircuit point circuit = point `elem` circuit
    circuit1 = unwrapMaybe $ find (isPointInCircuit p1) circuits
    circuit2 = unwrapMaybe $ find (isPointInCircuit p2) circuits

unwrapMaybe maybe =
  case maybe of
    Nothing -> error "Cloudflare moment"
    Just x -> x

readToInt :: String -> Int
readToInt = read

constructGraph :: [Point] -> Graph
constructGraph [] = []
constructGraph (p : pRest) = map (\p2 -> (p, euclideanDistance p p2, p2)) pRest ++ constructGraph pRest

euclideanDistance :: Point -> Point -> Float
euclideanDistance (x1, y1, z1) (x2, y2, z2) = sqrt . fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

split :: (Eq a) => a -> [a] -> [[a]]
split el eLs = split' el (eLs, [], [])

split' :: (Eq a) => a -> ([a], [a], [[a]]) -> [[a]]
split' el ([], [], resultLs) = resultLs
split' el ([], currLs, resultLs) = resultLs ++ [currLs]
split' el (e : eRest, currLs, resultLs) =
  if el == e
    then split' el (eRest, [], resultLs ++ [currLs])
    else split' el (eRest, currLs ++ [e], resultLs)
