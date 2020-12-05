getRow :: String -> Int
getRow x = find (take 7 x) [0..127]

getColumn :: String -> Int
getColumn x = find (drop 7 x) [0..7]

getSeatID :: String -> Int
getSeatID x = row * 8 + column 
    where   row = getRow x
            column = getColumn x

find :: String -> [Int] -> Int
find [] (x:_) = x
find (d:dr) x
    | d `elem` ['B','R'] = find dr (drop half x)
    | d `elem` ['F','L'] = find dr (take half x)
    where half = length x `div` 2

isMissingSeat :: Int -> Int -> [Int] -> Bool
isMissingSeat row column  seats = seatID `notElem` seats && seatID - 1 `elem` seats && seatID + 1 `elem` seats where seatID = row * 8 + column
main = do
    content <- readFile "input.txt"
    let seatIDs = map getSeatID $ lines content
    print $ maximum seatIDs
    print $ head  [row * 8 + column | row <- [0..127], column <- [0..7], isMissingSeat row column seatIDs]