import Data.List

answers :: [String] -> [[Char]]
answers [] = []
answers x = answer : remaining
    where   answer = concat $ takeWhile (/= "") x
            remaining = answers (drop (length (takeWhile (/= "") x) + 1) x)

main = do
    content <- readFile "input.txt"
    let a = answers $ lines content
    print $ sum $ map (length . nub) a