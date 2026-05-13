import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)


getKey :: IO Int
getKey = do
  line <- getLine
  return $ read line


getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x,y) =
  [(x+dx,y+dy) | dx <- [-1,0,1],
                 dy <- [-1,0,1],
                 (dx,dy) /= (0,0)]


type Grid = Map (Int, Int) Int

expand :: Grid -> Int -> Int -> (Grid, Int)
expand grid n m = (grid', m')
  where
    (x',y') = ((n-1)`div`2, -(n-3)`div`2)

    xx = x'
    yy = y'+(n-2)
    x = xx-(n-1)
    y = y'-1

    indices = 
      [(xx,z) | z <- [y'..yy]] ++
      [(z,yy) | z <- [xx-1,xx-2..x]] ++
      [ (x,z) | z <- [yy-1,yy-2..y]] ++
      [ (z,y) | z <- [x+1..xx]]
    
    (grid', m') = foldl go (grid, m) indices

    go (grid, m) (x,y) =
      let
        neighbors = getNeighbors (x,y)
        neighborSum = foldl go' 0 neighbors
        go' acc (x,y) = acc + (Map.findWithDefault 0 (x,y) grid)
      in
        (Map.insert (x,y) neighborSum grid, m+1)


find :: Int -> Grid -> Maybe Int
find target grid =
  let
    result = foldl go maxBound (Map.elems grid)
    go supremum v
      | v > target && v < supremum = v
      | otherwise = supremum
  in
    if result == maxBound
       then Nothing
       else Just result


distance :: (Int, Int) -> Int
distance (x,y) = abs x + abs y


main = do
  key <- getKey

  let
    grid = Map.singleton (0,0) 1
    m = 2

    supremum = foldr go (const 0) [3,5..] (grid, m)

    go n rest (grid, m) =
      case find key grid of
        Just v -> v
        Nothing -> rest $ expand grid n m

  in
    print supremum
