module Main where 
import Data.List (sort)

splitColumns :: String -> ([Int], [Int])
splitColumns = unzip . map (\[x, y] -> (read x :: Int, read y :: Int)) . map words . lines

main :: IO ()
main = do 
  content <- readFile "input.txt"
  let (col1, col2) = splitColumns content
      part1 = sum $ zipWith (\x y -> abs (x - y)) (sort col1) (sort col2)
      part2 = sum $ map (\x -> x * (length $ filter (== x) col2)) col1
  print (part1, part2)
