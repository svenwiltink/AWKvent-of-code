tree :: (Int, String) -> Int -> Int -> Bool
tree (x, slope) dX dY = '#' == ( cycle slope !! (x * dX `div` dY)) && x `mod` dY == 0

trees :: String -> (Int, Int) -> Int
trees input (dX, dY) = length $ [x | x <- zip [0,1..] . lines $ input, tree x dX dY]

main = do
    content <- readFile "input.txt"
    print $ trees content (3, 1)
    print $ product $ map (trees content) [(1,1), (3,1), (5,1), (7,1), (1,2)]