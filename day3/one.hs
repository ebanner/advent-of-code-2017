import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)


getKey :: IO Int
getKey = do
  line <- getLine
  return $ read line


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
      [(xx,k) | k <- [y'..yy]] ++
      [(k,yy) | k <- [xx-1,xx-2..x]] ++
      [ (x,k) | k <- [yy-1,yy-2..y]] ++
      [ (k,y) | k <- [x+1..xx]]
    
    (grid', m') = foldl go (grid, m) indices

    go (grid, m) (y,x) = 
      (Map.insert (y,x) m grid, m+1)


find :: Int -> Grid -> Maybe (Int, Int)
find target grid = result
  where
    result = foldr go Nothing $ Map.toList grid
    go (k,v) rest
      | v == target = Just k
      | otherwise = rest


distance :: (Int, Int) -> Int
distance (x,y) = abs x + abs y


main = do
  key <- getKey

  let
    grid = Map.singleton (0,0) 1
    m = 2

    (grid', _) = foldr go id [3,5..] (grid, m)

    go n rest (grid, m) =
      case find key grid of
        Just (x,y) -> (grid, m)
        Nothing -> rest $ expand grid n m

  in
    print $ 
      distance $
        fromJust $
          find key grid'
