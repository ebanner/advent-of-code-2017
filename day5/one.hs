import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


getJumpOffsets :: IO (Map Int Int)
getJumpOffsets = do
  file <- readFile "input"
  let 
    lines' = lines file
    jumpOffsets = map read lines'

  return $ Map.fromList $ zip [0..] jumpOffsets


run :: Map Int Int -> Int -> Int
run jumpOffsets pc
  | pc < 0 || length jumpOffsets <= pc = 0
  | otherwise = (+1) $ run (Map.adjust (+1) pc jumpOffsets) (pc + (jumpOffsets Map.! pc))


main = do
  jumpOffsets <- getJumpOffsets

  run jumpOffsets 0
