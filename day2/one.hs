parse :: String -> [Int]
parse line = map read $ words line


getRows :: IO [[Int]]
getRows = do
  file <- readFile "input"
  let lines' = lines file
  return $ map parse lines'


main = do
  rows <- getRows

  let
    differences = map difference rows
    difference row =
      maximum row - minimum row
  in
    print $ sum differences
