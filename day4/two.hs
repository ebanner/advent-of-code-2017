import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


getPassphrases :: IO [[String]]
getPassphrases = do
  file <- readFile "input"
  let
    lines' = lines file
    passphrases = map words lines'
  return passphrases


getCounts :: String -> Map Char Int
getCounts s = counts
  where
    counts = foldl go Map.empty s
    go counts c = Map.insertWith (+) c 1 counts


isAnagram :: String -> String -> Bool
isAnagram a b = getCounts a == getCounts b


isValid :: [String] -> Bool
isValid passphrase = result
  where
    n = length passphrase
    indices = [(i,j) | i <- [0..n-2], j <- [i+1..n-1]]

    result = foldr containsAnagram True indices
    containsAnagram (i,j) rest
      | isAnagram (passphrase!!i) (passphrase!!j) = False
      | otherwise = rest


main = do
  passphrases <- getPassphrases

  print $ length $ filter isValid passphrases
