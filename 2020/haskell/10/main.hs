import Data.List

parse :: String -> [Int]
parse = map read . lines

calculateDiff :: [Int] -> [Int]
calculateDiff x = zipWith (calculateDiffForNumber x) [0 .. ] x

calculateDiffForNumber :: [Int] -> Int -> Int -> Int
calculateDiffForNumber x index number
    | first = number
    | otherwise = number - x !! (index -1)
    where first = index == 0

calculateOrders :: [Int] -> Int
calculateOrders x = product filtered
    where filtered = map possibilities $ filter (1  `elem` ) $ group x

possibilities :: [Int] -> Int
possibilities x
    | l < 4 = 2 ^ (l-1)
    | l == 4 = 7 -- not 8 because you can't remove all three
    where l = length x

main = do
    content <- readFile "input.txt"
    let diffs = calculateDiff (sort $ parse content) ++ [3]
    let oneDiff = length $ filter (== 1) diffs
    let threeDiff = length $ filter (== 3) diffs
    print $ oneDiff * threeDiff
    print $ calculateOrders diffs
