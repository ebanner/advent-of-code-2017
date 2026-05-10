import Data.Char (digitToInt)


getDigits :: IO [Int]
getDigits = do
  line <- readFile "input"
  return $ map digitToInt $ init line


main = do
  digits <- getDigits

  let
    n = length digits
    mid = n `div` 2
    matchingIndices = filter doesMatch [0..n-1]
    doesMatch i = digits !! i == digits !! j
      where
        j = (i+mid) `mod` n

    matchingDigits = map (digits!!) matchingIndices
  in
    print $ sum matchingDigits
