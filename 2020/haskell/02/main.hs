parseLine :: String -> (Int, Int, Char, String)
parseLine line = do
    let parts = words line
    let min = read $ takeWhile (/='-') $ head parts :: Int
    let max = read $ tail . dropWhile (/='-') $ head parts :: Int
    let char = head $ takeWhile (/=':') $ parts !! 1
    (min, max, char, parts !! 2)

validPassword :: String -> Bool
validPassword x = do
    let (min, max, char, pass) = parseLine x
    let num = length $ [y | y <- pass, y == char]
    num >= min && num <= max

validPassword2 :: String -> Bool
validPassword2 x = do
    let (pos1, pos2, char, pass) = parseLine x
    (if pass !! (pos1 - 1) == char then 1 else 0) + (if pass !! (pos2 - 1) == char then 1 else 0) == 1

main = do
    content <- readFile "input.txt"
    print $ length $ filter validPassword $ lines content
    print $ length $ filter validPassword2 $ lines content