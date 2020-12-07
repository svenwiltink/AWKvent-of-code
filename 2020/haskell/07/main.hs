import Data.List.Split
import qualified Data.HashMap.Strict as HashMap

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

main = do
    content <- readFile "input.txt"
    let baggage = HashMap.fromList $ map parse $ lines content
    print$ length $ HashMap.keys $ HashMap.filter (validPath "shiny gold" baggage) baggage
