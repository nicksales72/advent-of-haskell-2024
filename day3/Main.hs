module Main where
import Text.Regex.TDFA

findMulTuples :: String -> [(Int, Int)]
findMulTuples text = 
  map parseTuple (getAllTextMatches (text =~ "mul\\((-?[0-9]+),(-?[0-9]+)\\)") :: [String])
  where
    parseTuple str = 
      case str =~ "mul\\((-?[0-9]+),(-?[0-9]+)\\)" :: (String, String, String, [String]) of
        (_, _, _, [x, y]) -> (read x :: Int, read y :: Int)
        _ -> error $ "Failed to parse: " ++ str  

main :: IO ()
main = do
  text <- readFile "input.txt"
  let mulTuples = findMulTuples text
  let part1 = sum $ map (uncurry(*)) mulTuples
  print part1
