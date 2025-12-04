import System.Environment

main :: IO ()
main = getContents >>= print . getAccessableRolls . gridToCoords . lines

gridToCoords :: [String] -> [(Int,Int)]
gridToCoords = snd .
    foldl
        (\(i,acc1) a ->
            (i + 1, acc1 ++ snd
                (foldl
                    (\(j,acc2) b ->
                        (j + 1, if b == '@'
                                then acc2 ++ [(i, j)]
                                else acc2))
        (0,[]) a)))
    (0,[])

getAccessableRolls :: [(Int,Int)] -> Int
getAccessableRolls = getAccessableRolls' 0

getAccessableRolls' :: Int -> [(Int,Int)] -> Int
getAccessableRolls' acc al = acc + sizeDifference
  where
    newList = filter (\x -> numberOfSurroundingRolls al x >= 4) al
    numberOfSurroundingRolls bl e@(e1,e2) = foldl (\x y -> if y then x + 1 else x) 0 $
      map (`elem`bl) [(e1-1,e2-1),(e1-1,e2+0),(e1-1,e2+1),
                      (e1+0,e2-1),            (e1+0,e2+1),
                      (e1+1,e2-1),(e1+1,e2+0),(e1+1,e2+1)]
    sizeDifference = length al - length newList
