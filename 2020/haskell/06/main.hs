import qualified Data.HashSet as HashSet

answers :: [String] -> [[String]]
answers [] = []
answers x = answer : remaining
    where   (answer, r) = span (/= "") x
            remaining = answers $ drop 1 r

sameAnswer :: [String] -> Int
sameAnswer x = length $ foldl1 HashSet.intersection (map HashSet.fromList x)

main = do
    content <- readFile "input.txt"
    let a = answers $ lines content
    print $ sum $ map (HashSet.size . HashSet.fromList . concat) a
    print $ sum $ map sameAnswer a