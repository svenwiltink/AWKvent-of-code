import Data.List
import qualified Data.HashSet as HashSet

answers :: [String] -> [[String]]
answers [] = []
answers x = answer : remaining
    where   answer = takeWhile (/= "") x
            remaining = answers $ drop (length answer + 1) x

sameAnswer :: [String] -> Int
sameAnswer x = length $ foldl1 HashSet.intersection (map HashSet.fromList x)

main = do
    content <- readFile "input.txt"
    let a = answers $ lines content
    print $ sum $ map (length . nub . concat) a
    print $ sum $ map sameAnswer a