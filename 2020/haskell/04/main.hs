passports :: [String] -> [[String]]
passports [] = []
passports x = pass : remaining
    where 
        pass = concatMap words $ takeWhile (/= "") x
        remaining = passports (drop (length (takeWhile (/= "") x) + 1) x)
        
main = do
    content <- readFile "input.txt"
    let pass = passports $ lines content
    print $ length [x | x <- pass, (length x == 8) || (length x == 7) && "cid" `notElem` map (take 3) x]