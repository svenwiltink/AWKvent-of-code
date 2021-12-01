main = do
    content <- readFile "input.txt"
    let nums =  map read $ lines content :: [Int]
    let part1 = sum [ 1  | (i,num) <- zip [0..] nums, i > 0, nums!!(i-1) < num]
    print part1
    let sums = [ sum [num, nums!!(i+1), nums!!(i+2)] | (i,num) <- zip [0..] nums, i < (length nums) - 2]
    let part2 =  sum [ 1  | (i,num) <- zip [0..] sums, i > 0, sums!!(i-1) < num]
    print part2