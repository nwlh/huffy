module Main where

import Data.List

sortFreq :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortFreq [] = []
sortFreq [x] = [x] 
sortFreq xs = sortBy (\(_,a) (_,b) -> flip compare a b) xs

analyzeString :: String -> [(Char, Int)]

main :: IO ()
main = do
  putStrLn "PRIORITY QUEUE: "
  print $ sortFreq [("a", 2), ("b", 5), ("c", 3)]
