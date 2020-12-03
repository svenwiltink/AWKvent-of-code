tree (x, slope) = hit
    where hit = '#' == ( cycle slope !! (x * 3))

main = do
    content <- readFile "input.txt"
    print $ length $ [x | x <- zip [0,1..] . lines $ content, tree x]