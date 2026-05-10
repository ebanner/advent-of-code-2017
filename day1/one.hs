import Data.Char (digitToInt)


getDigits :: IO [Int]
getDigits = do
  line <- readFile "input"
  return $ map digitToInt $ init line


main = do
  digits <- getDigits

  sum $
    map sum $
      filter
        (uncurry (==)) $
        zip digits $ tail digits ++ [head digits]

