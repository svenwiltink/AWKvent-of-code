
import Data.List.Split

validPassword :: String -> Bool
validPassword x = do
    let parts = words x
    let min = read $ takeWhile (/='-') $ head parts :: Int
    let max = read $ tail . dropWhile (/='-') $ head parts :: Int
    let char = head $ takeWhile (/=':') $ parts !! 1
    let num = length $ [y | y <- parts !! 2, y == char]
    num >= min && num <= max

main = do
    content <- readFile "input.txt"
    print $ length $ [x | x <- lines content, validPassword x]