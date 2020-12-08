import qualified Data.HashSet as Set

data Instruction = Instruction
  { name :: String,
    parameters :: [String]
  }
  deriving (Show, Eq)

data Machine = Machine
  { pc :: Int,
    acc :: Int,
    instructions :: [Instruction],
    replacement :: Int
  }
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction x = Instruction {name = name, parameters = parameters}
  where
    w = words x
    name = head w
    parameters = tail w

parseMachineCode :: String -> Machine
parseMachineCode x = Machine {pc = 0, acc = 0, replacement = -1, instructions = instructions}
  where
    instructions = map parseInstruction $ lines x

runMachineUntilLoop :: Machine -> Machine
runMachineUntilLoop = runUntilVisited Set.empty

runUntilVisited :: Set.HashSet Int -> Machine -> Machine
runUntilVisited visited m
  | visitedBefore = m
  | programEnded = m
  | otherwise = runUntilVisited (Set.insert (pc m) visited) $ stepMachine m
  where
    visitedBefore = Set.member (pc m) visited
    programEnded = pc m >= length (instructions m)

stepMachine :: Machine -> Machine
stepMachine m
  | replace == cpc = runInstruction (replaceInstruction i) m
  | otherwise = runInstruction i m
  where
    cpc = pc m
    replace = replacement m
    i = instructions m !! pc m

replaceInstruction :: Instruction -> Instruction
replaceInstruction i@Instruction {name = "nop"} = i {name = "jmp"}
replaceInstruction i@Instruction {name = "jmp"} = i {name = "nop"}

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

findCorrectReplacement :: Machine -> Machine
findCorrectReplacement m = snd $ head matching
  where
    machines = zip (repeat m) [0 .. (length (instructions m) -1)]
    results = map (uncurry tryReplacement) machines
    matching = filter fst results

tryReplacement :: Machine -> Int -> (Bool, Machine)
tryReplacement m inst
  | opcode `elem` ["nop", "jmp"] = runReplacement m inst
  | otherwise = (False, m) -- never try this variant. It will never succeed
  where
    opcode = name $ instructions m !! inst

runReplacement :: Machine -> Int -> (Bool, Machine)
runReplacement m i = (ok, finishedMachine)
  where
    finishedMachine = runMachineUntilLoop m {replacement = i}
    ok = pc finishedMachine == length (instructions m)

main = do
  content <- readFile "input.txt"
  let machine = parseMachineCode content
  print $ acc $ runMachineUntilLoop machine
  print $ acc $ findCorrectReplacement machine