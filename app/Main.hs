module Main where

import Data.List

sortFreq :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortFreq [] = []
sortFreq [x] = [x] 
sortFreq xs = sortBy (\(_,a) (_,b) -> flip compare a b) xs

analyzeString :: String -> [(Char, Int)]
analyzeString [] = []
analyzeString s@(x:xs) = (x, count x s) : analyzeString xs where
  count c = length . filter (==c)

filterFreq :: Eq a => [(a, b)] -> [(a, b)]
filterFreq [] = []
filterFreq xs = nubBy (\(a,_) (b,_) -> a == b) xs

main :: IO ()
main = do
  putStrLn "PRIORITY QUEUE: "
  print $ filterFreq $ sortFreq $ analyzeString "aabcbaab"
