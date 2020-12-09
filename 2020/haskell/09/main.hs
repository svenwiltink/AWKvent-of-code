import Data.Maybe

parse :: String -> [Int]
parse x = map read $ lines x

valid :: [Int] -> Int -> Int -> Bool
valid input preamble index = not (null matching)
  where
    numbers = take preamble $ drop (index - preamble) input
    number = input !! index
    matching = [(x, y) | x <- numbers, y <- numbers, x /= y, x + y == number]

firstInvalid :: [Int] -> Int -> Int
firstInvalid input preamble = input !! head invalid
  where
    invalid = filter (not . valid input preamble) [preamble .. (length input -1)]

checkRange :: [Int] -> Int -> Int -> Maybe [Int]
checkRange input goal start
  | not (null range) && last range == goal = Just $ take (length range) remainder
  | otherwise = Nothing
  where
    remainder = drop start input
    range = takeWhile (<= goal) $ scanl1 (+) remainder

findRange :: [Int] -> Int -> [Int]
findRange input target = fromJust $ head correct
  where
    checked = map (checkRange input target) [0 .. length input -1]
    correct = filter (\x -> isJust x && length (fromJust x) > 1) checked

main = do
  content <- readFile "input.txt"
  let num = parse content
  let invalid = firstInvalid num 25
  print invalid
  let range = findRange num invalid
  print $ minimum range + maximum range