main = do
    content <- readFile "input.txt"
    let numbers = map read (lines content :: [String]) :: [Integer]
    let x = [x*y | x <- numbers, y <- numbers, x+y == 2020]
    let z = [x*y*z | x <- numbers, y <- numbers, z <- numbers, x+y+z == 2020]
    print (head x)
    print (head z)