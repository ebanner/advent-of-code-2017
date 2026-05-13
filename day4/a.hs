import qualified Data.Set as Set


getPassphrases :: IO [[String]]
getPassphrases = do
  file <- readFile "input"
  let
    lines' = lines file
    passphrases = map words lines'
  return passphrases


isValid :: [String] -> Bool
isValid passphrase = result
  where
    result = foldr go (const True) passphrase Set.empty
    go word rest words
      | Set.member word words = False
      | otherwise = rest $ Set.insert word words


main = do
  passphrases <- getPassphrases

  print $ length $ filter isValid passphrases
