import Data.List.Split
import qualified Data.HashMap.Strict as HashMap


data Bag = Bag {
    colour :: String,
    contents :: [(Int, String)]
}

parseBag :: [String] -> (Int, String)
parseBag x = (amount, content)
    where   amount = read $ head x :: Int
            content = unwords $ take 2 $ tail x

parseContents :: [String] -> [(Int, String)]
parseContents ("no":_) = []
parseContents x = map parseBag (chunksOf 4 x)

parse :: String -> (String, [(Int, String)])
parse x = (bag, contents)
    where   parts = words x
            bag = unwords $ take 2 parts
            contents = parseContents $ drop 4 parts

-- medium cursed function
validPath :: String -> HashMap.HashMap String [(Int, String)] -> [(Int, String)]-> Bool
validPath item baggage children  = directPath || indirectpath
    where   directPath = any (\(_, name) -> name == item) children
            indirectpath = any (\(_, name) -> validPath item baggage (HashMap.lookupDefault [] name baggage)) children


totalWeight :: String -> HashMap.HashMap String [(Int, String)] -> Int
totalWeight item baggage = 1 + c
    where   children  = HashMap.lookupDefault [] item baggage
            c = weightChildren children baggage

weightChildren :: [(Int, String)] -> HashMap.HashMap String [(Int, String)] -> Int
weightChildren [] _ = 0
weightChildren x baggage = childWeight 
    where  childWeight = foldl (\acc (amount, name) -> acc + (amount * totalWeight name baggage)) 0 x

main = do
    content <- readFile "input.txt"
    let baggage = HashMap.fromList $ map parse $ lines content
    print $ length $ HashMap.keys $ HashMap.filter (validPath "shiny gold" baggage) baggage
    print $ totalWeight "shiny gold" baggage - 1
