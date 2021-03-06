passports :: [String] -> [[String]]
passports [] = []
passports x = pass : remaining
    where   pass = concatMap words $ takeWhile (/= "") x
            remaining = passports (drop (length (takeWhile (/= "") x) + 1) x)

validHeight :: String -> Bool
validHeight height = (unit == "cm" && h >= 150 && h <= 193) || (unit == "in" && h >= 59 && h <= 76)
    where   unit = reverse $ take 2 $ reverse height
            h = read $ reverse $ drop 2 $ reverse height :: Int

validHairColour :: String -> Bool
validHairColour colour = startOK && suffixOK
    where   startOK = head colour == '#'
            suffixOK = (length (tail colour) == 6) && all (`elem` ['0'..'9']++['a'..'f']) (tail colour)

validProperty :: String -> String -> Bool
validProperty "byr" year = x >= 1920 && x <= 2002 where x = read year::Int
validProperty "iyr" year = x >= 2010 && x <= 2020 where x = read year::Int
validProperty "eyr" year = x >= 2020 && x <= 2030 where x = read year::Int
validProperty "hgt" height = validHeight height
validProperty "hcl" hc = validHairColour hc
validProperty "ecl" ec = ec `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validProperty "pid" pid = length pid == 9 && all (`elem` ['0'..'9']) pid
validProperty "cid" _ = True

validBlock :: String -> Bool
validBlock i = validProperty (take 3 i) (drop 4 i)

validPassport :: [String] -> Bool
validPassport x = enoughFields x && all validBlock x

enoughFields :: [String] -> Bool
enoughFields x = (length x == 8) || (length x == 7 && "cid" `notElem` map (take 3) x)

main = do
    content <- readFile "input.txt"
    let pass = passports $ lines content
    print $ length $ filter enoughFields pass
    print $ length $ filter validPassport pass