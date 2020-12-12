data Ferry = Ferry { angle :: Int, x :: Int, y :: Int} deriving (Show)

run :: Ferry -> [String] -> Ferry
run = foldl runInstrunction

runInstrunction :: Ferry -> String -> Ferry
runInstrunction f ('N' : n) = f {y = newY} where newY = y f + read n
runInstrunction f ('S' : n) = f {y = newY} where newY = y f - read n
runInstrunction f ('E' : n) = f {x = newX} where newX = x f + read n
runInstrunction f ('W' : n) = f {x = newX} where newX = x f - read n
runInstrunction f ('R' : n) = f {angle = newA} where newA = (angle f + read n + 360) `mod` 360
runInstrunction f ('L' : n) = f {angle = newA} where newA = (angle f - read n + 360) `mod` 360
runInstrunction f@Ferry {angle = 0} ('F' : n) = f {y = newY} where newY = y f + read n
runInstrunction f@Ferry {angle = 90} ('F' : n) = f {x = newX} where newX = x f + read n
runInstrunction f@Ferry {angle = 180} ('F' : n) = f {y = newY} where newY = y f - read n
runInstrunction f@Ferry {angle = 270} ('F' : n) = f {x = newX} where newX = x f - read n

main = do
  content <- readFile "input.txt"
  let f = run Ferry {angle = 90, x = 0, y = 0} $ lines content
  print $ abs (x f) + abs (y f)