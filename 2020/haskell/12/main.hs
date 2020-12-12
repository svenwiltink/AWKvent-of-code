data Ferry = Ferry {angle :: Int, x :: Int, y :: Int, wx :: Int, wy :: Int} deriving (Show)

runOne :: Ferry -> [String] -> Ferry
runOne = foldl runInstrunctionOne

runInstrunctionOne :: Ferry -> String -> Ferry
runInstrunctionOne f ('N' : n) = f {y = newY} where newY = y f + read n
runInstrunctionOne f ('S' : n) = f {y = newY} where newY = y f - read n
runInstrunctionOne f ('E' : n) = f {x = newX} where newX = x f + read n
runInstrunctionOne f ('W' : n) = f {x = newX} where newX = x f - read n
runInstrunctionOne f ('R' : n) = f {angle = newA} where newA = (angle f + read n + 360) `mod` 360
runInstrunctionOne f ('L' : n) = f {angle = newA} where newA = (angle f - read n + 360) `mod` 360
runInstrunctionOne f@Ferry {angle = 0} ('F' : n) = f {y = newY} where newY = y f + read n
runInstrunctionOne f@Ferry {angle = 90} ('F' : n) = f {x = newX} where newX = x f + read n
runInstrunctionOne f@Ferry {angle = 180} ('F' : n) = f {y = newY} where newY = y f - read n
runInstrunctionOne f@Ferry {angle = 270} ('F' : n) = f {x = newX} where newX = x f - read n

runTwo :: Ferry -> [String] -> Ferry
runTwo = foldl runInstrunctionTwo

runInstrunctionTwo :: Ferry -> String -> Ferry
runInstrunctionTwo f ('N' : n) = f {wy = newY} where newY = wy f + read n
runInstrunctionTwo f ('S' : n) = f {wy = newY} where newY = wy f - read n
runInstrunctionTwo f ('E' : n) = f {wx = newX} where newX = wx f + read n
runInstrunctionTwo f ('W' : n) = f {wx = newX} where newX = wx f - read n

runInstrunctionTwo f ('R' : "90") = f {wx=newX, wy=newY} 
    where newX = wy f
          newY = -wx f

runInstrunctionTwo f ('R' : "180") = f {wx=newX, wy=newY} 
    where newX = -wx f
          newY = -wy f

runInstrunctionTwo f ('R' : "270") = f {wx=newX, wy=newY} 
    where newX = -wy f
          newY = wx f

runInstrunctionTwo f ('L' : "90") = runInstrunctionTwo f "R270"
runInstrunctionTwo f ('L' : "180") = runInstrunctionTwo f "R180"
runInstrunctionTwo f ('L' : "270") = runInstrunctionTwo f "R90"

runInstrunctionTwo f('F' : n) = f {x = newX, y = newY}
    where m = read n :: Int
          newX = x f + wx f * m
          newY = y f + wy f * m

main = do
  content <- readFile "input.txt"
  let f = runOne Ferry {angle = 90, x = 0, y = 0, wx = 0, wy = 0} $ lines content
  print $ abs (x f) + abs (y f)
  let f2 = runTwo Ferry {angle = 90, x = 0, y = 0, wx = 10, wy = 1} $ lines content
  print $ abs (x f2) + abs (y f2)