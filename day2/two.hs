import Data.List (sort, sortOn)
import Data.Ord (Down(..))


parse :: String -> [Int]
parse line = map read $ words line


getRows :: IO [[Int]]
getRows = do
  file <- readFile "input"
  let lines' = lines file
  return $ map parse lines'


getDivisors :: [Int] -> (Int, Int)
getDivisors row = (q, r)
  where
    n = length row
    indices = [(i,j) | i <- [0..n-2], j <- [i+1..n-1]]
    (q, r) = foldr go (-1,-1) indices
    go (i,j) rest
      | row !! i `mod` row !! j == 0 = (row!!i, row!!j)
      | otherwise = rest


quotient :: [Int] -> Int
quotient row = q `div` r
  where
    (q, r) = getDivisors row


main = do
  rows <- getRows

  let
    sortedRows = map (sortOn Down) rows
    quotients = map quotient sortedRows
  in
    print $ sum quotients
