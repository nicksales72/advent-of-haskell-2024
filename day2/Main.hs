module Main where
import Data.List (sort, inits, tails)

inputList :: String -> [[Int]]
inputList = map (map read . words) . lines

isMonotonic :: [Int] -> Bool
isMonotonic xs = xs == sortXS || xs == reverse sortXS
  where sortXS = sort xs

adjDifference :: [Int] -> Bool 
adjDifference [x, y] = abs (x - y) >= 1 && abs (x - y) <= 3
adjDifference (x:y:xs)
  | abs (x - y) >= 1 && abs (x - y) <= 3 = adjDifference (y:xs)
  | otherwise                            = False

satisfiesCond :: [Int] -> Bool
satisfiesCond xs = adjDifference xs && isMonotonic xs

removeOne :: [Int] -> [[Int]]
removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]] 

checkRemoval :: [Int] -> Bool
checkRemoval xs = any satisfiesCond (removeOne xs)

main :: IO ()
main = do 
  contents <- readFile "input.txt"
  let contentList = inputList contents
      part1 = length $ filter satisfiesCond contentList
      part2 = length $ filter checkRemoval contentList
  print (part1, part2)
