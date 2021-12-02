instruction :: String -> (String, Int)
instruction input = (instruction, amount)
    where   instruction = (words input)!!0
            amount = read $ (words input)!!1 :: Int

part1instruction :: (Int, Int) -> (String, Int) -> (Int, Int)
part1instruction (x, y) ("forward", num)    = (x+num, y) 
part1instruction (x, y) ("up", num)         = (x, y-num) 
part1instruction (x, y) ("down", num)       = (x, y+num) 

part2instruction :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
part2instruction (x, y, aim) ("forward", num)    = (x+num, y + aim * num, aim) 
part2instruction (x, y, aim) ("up", num)         = (x, y, aim - num) 
part2instruction (x, y, aim) ("down", num)       = (x, y, aim + num) 

main = do
    content <- readFile "input.txt"
    let instructions =  map instruction $ lines content
    let (x1,y1) = foldl part1instruction (0,0) instructions
    print $ x1 * y1
    let (x2,y2, _) = foldl part2instruction (0,0,0) instructions
    print $ x2 * y2