import Data.List

answers :: [String] -> [[String]]
answers [] = []
answers x = answer : remaining
    where   answer = takeWhile (/= "") x
            remaining = answers (drop (length (takeWhile (/= "") x) + 1) x)

sameAnswer :: [String] -> Int
sameAnswer [first] = length $ nub first
sameAnswer (first:tail) = length $ nub $ concat a
    where a = map (filter (`elem` first)) tail
main = do
    content <- readFile "input.txt"
    let a = answers $ lines content
    print $ sum $ map (length . nub . concat) a
    print $ sum $ map sameAnswer a
