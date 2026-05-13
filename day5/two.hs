{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


getJumpOffsets :: IO (Map Int Int)
getJumpOffsets = do
  file <- readFile "input"
  let 
    lines' = lines file
    jumpOffsets = map read lines'

  return $ Map.fromList $ zip [0..] jumpOffsets


run :: Map Int Int -> Int -> Int -> Int
run !jumpOffsets !pc !numSteps
  | pc < 0 || length jumpOffsets <= pc = numSteps
  | otherwise = 
      run
        (Map.adjust \jo -> if jo >= 3 then jo-1 else jo+1) pc jumpOffsets)
        (pc + (jumpOffsets Map.! pc))
        (numSteps+1)


main = do
  jumpOffsets <- getJumpOffsets

  print $ run jumpOffsets 0 0
