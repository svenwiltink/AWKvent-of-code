parse :: String -> [Int]
parse x = map read $ lines x

valid :: [Int] -> Int -> Int -> Bool
valid input preamble index = not (null matching)
    where numbers = take preamble $ drop (index-preamble) input
          number = input !! index
          matching = [(x,y) | x <- numbers, y <- numbers, x /= y, x + y == number]

firstInvalid :: [Int] -> Int -> Int
firstInvalid input preamble = input !! head invalid
    where invalid = filter (not . valid input preamble) [preamble..(length input -1)]

main = do
    content <- readFile "input.txt"
    let num = parse content
    print $ firstInvalid num 25