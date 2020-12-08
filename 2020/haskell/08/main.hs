import qualified Data.HashSet as Set

data Instruction = Instruction
  { name :: String,
    parameters :: [String]
  }
  deriving (Show, Eq)

data Machine = Machine
  { pc :: Int,
    acc :: Int,
    instructions :: [Instruction]
  }
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction x = Instruction {name = name, parameters = parameters}
  where
    w = words x
    name = head w
    parameters = tail w

parseMachineCode :: String -> Machine
parseMachineCode x = Machine {pc = 0, acc = 0, instructions = instructions}
  where
    instructions = map parseInstruction $ lines x

runMachineUntilLoop :: Machine -> Machine
runMachineUntilLoop = runUntilVisited Set.empty

runUntilVisited :: Set.HashSet Int -> Machine -> Machine
runUntilVisited visited m
  | visitedBefore = m
  | otherwise = runUntilVisited (Set.insert (pc m) visited) $ stepMachine m
  where
    visitedBefore = Set.member (pc m) visited

stepMachine :: Machine -> Machine
stepMachine m = runInstruction i m
  where
    i = instructions m !! pc m

runInstruction :: Instruction -> Machine -> Machine
runInstruction Instruction {name = "nop"} m = m {pc = pc m + 1}
runInstruction Instruction {name = "acc", parameters = [n]} m = m {pc = newPC, acc = newACC}
  where
    newPC = pc m + 1
    newACC = acc m + signedInt n
runInstruction Instruction {name = "jmp", parameters = [n]} m = m {pc = newPC}
  where
    newPC = pc m + signedInt n

signedInt :: String -> Int
signedInt ('+' : i) = read i
signedInt i = read i :: Int

main = do
  content <- readFile "input.txt"
  let machine = parseMachineCode content
  print $ acc $ runMachineUntilLoop machine