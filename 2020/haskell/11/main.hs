data Position = Floor | Empty | Occupied deriving (Show, Eq)

data Plan = Plan
  { width :: Int,
    height :: Int,
    positions :: [[Position]]
  }
  deriving (Show, Eq)

parse :: String -> Plan
parse x = Plan {width = w, height = h, positions = p}
  where
    l = lines x
    w = length $ head l
    h = length l
    p = map parsePos l

parsePos :: String -> [Position]
parsePos = map pos

pos :: Char -> Position
pos 'L' = Empty
pos '#' = Occupied
pos '.' = Floor

adjacent :: Plan -> Int -> Int -> [Position]
adjacent p x y = top ++ current ++ bottom
  where
    top = getRow p x (y -1)
    current = getRow p x y
    bottom = getRow p x (y + 1)

getRow :: Plan -> Int -> Int -> [Position]
getRow p x y
  | rowExists = getExistingRow p x y
  | otherwise = []
  where
    rowExists = y >= 0 && y < height p

getExistingRow :: Plan -> Int -> Int -> [Position]
getExistingRow p x y = slice (max 0 (x -1)) (min (width p) (x + 1)) row
  where
    row = positions p !! y

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

tickOne :: Plan -> Plan
tickOne old = old {positions = newPos}
  where
    newPos = zipWith (tickRowOne old) [0 ..] $ positions old

tickRowOne :: Plan -> Int -> [Position] -> [Position]
tickRowOne old y = zipWith (tickTileOne old y) [0 ..]

tickTileOne :: Plan -> Int -> Int -> Position -> Position
tickTileOne _ _ _ Floor = Floor
tickTileOne old y x Empty
  | numOccupied == 0 = Occupied
  | otherwise = Empty
  where
    numOccupied = length $ filter (== Occupied) $ adjacent old x y
tickTileOne old y x Occupied
  | numOccupied > 4 = Empty
  | otherwise = Occupied
  where
    numOccupied = length $ filter (== Occupied) $ adjacent old x y


tickTwo :: Plan -> Plan
tickTwo old = old {positions = newPos}
  where
    newPos = zipWith (tickRowTwo old) [0 ..] $ positions old

tickRowTwo :: Plan -> Int -> [Position] -> [Position]
tickRowTwo old y = zipWith (tickTileTwo old y) [0 ..]

tickTileTwo :: Plan -> Int -> Int -> Position -> Position
tickTileTwo _ _ _ Floor = Floor
tickTileTwo old y x Empty
  | numOccupied == 0 = Occupied
  | otherwise = Empty
  where
    numOccupied = length $ filter (== Occupied) $ adjacent old x y
tickTileTwo old y x Occupied
  | numOccupied > 4 = Empty
  | otherwise = Occupied
  where
    numOccupied = length $ filter (== Occupied) $ adjacent old x y

converge :: Plan -> (Plan->Plan)-> Plan
converge old tick
    | new == old = new
    | otherwise = converge new tick
    where new = tick old

main = do
  content <- readFile "input.txt"
  let plan = parse content
  let a = converge plan tickOne
  print $ sum $ map (length . filter (== Occupied)) $ positions a