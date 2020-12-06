import Data.List
import qualified Data.HashSet as HashSet

answers :: [String] -> [[String]]
answers [] = []
answers x = answer : remaining
    where   answer = takeWhile (/= "") x
            remaining = answers (drop (length (takeWhile (/= "") x) + 1) x)

sameAnswer :: [String] -> Int
sameAnswer [first] = length $ nub first
sameAnswer x = length $ intersectMultiple $ map HashSet.fromList x 
        
intersectMultiple :: [HashSet.HashSet Char] -> HashSet.HashSet Char
intersectMultiple [x] = x
intersectMultiple [x,t] = HashSet.intersection x t
intersectMultiple (x:tail) = HashSet.intersection x remaining
    where remaining = intersectMultiple tail

main = do
    content <- readFile "input.txt"
    let a = answers $ lines content
    print $ sum $ map (length . nub . concat) a
    print $ sum $ map sameAnswer a
