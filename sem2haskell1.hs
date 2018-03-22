--this is for testing purposes (to check if each function works)
module Haskell where
  import Debug.Trace
  import Data.Ord
  import Data.List
  import System.Random

  type Input  = Int
  type Output = Int

-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same
--                 number of tokens, jump to instruction t
  class (Eq cfg) => ProgrammableComputer cfg where
    initialise   :: Program -> [Input] -> cfg
    getOutput    :: cfg -> Output
    acceptState  :: Program -> cfg -> Bool
    doNextMove   :: Program -> cfg -> cfg
    runFrom      :: Program -> cfg -> cfg
    runProgram   :: Program -> [Input] -> cfg
    -- Default implementation
    runProgram p is = runFrom p (initialise p is)

-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same
--                 number of tokens, jump to instruction t
--
{- data Instruction
    = CLR {box :: Int}
    | INC {box :: Int}
    | JEQ {box1   :: Int,
         box2   :: Int,
         target :: Int}
    deriving (Eq, Show)

  type Program = [Instruction]

--boxes is of type int
  data BATConfig = BATConfig {
      boxes   :: [Int],
      counter :: Int
      } deriving (Eq)

  instance Show BATConfig where
    --minimal complete definitons
	show = show.toString[]
	--defualt definitions
-}
  --instance ProgrammableComputer BATConfig  where
   {- -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg
	-- need to check the paramaters as in the BATconfig doesn't match the box
	-- number of params /= to the number args in the type definition
	-- needs to be fixed after the first problems have been completed
    initialise p x = BATConfig box1[] p x
    -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool
    acceptState  :: Program -> cfg -> Bool
	acceptState cfg$(Program CLR INC JEQ)
	  = null BATConfig && null Instruction
    -  PROBLEM 5: doNextMove   :: Program -> cfg -> cfg
	-}
	{-
    doNextMove p cfg@(BATConfig b1 c)
      | null Instruction       = cfg
      | acceptState cfg        = cfg
      | otherwise              = (BATConfig b1' c')
	   where
	         (Program CLR INC JEQ)  = if (null n) then (Program CLR JEQ INC)
		                              else head n

			  n = [(Program CLR INC JEQ) !! n]

    -- PROBLEM 6: runFrom      :: Program -> cfg -> cfg
       runFrom p cfg@(BATConfig b1 c)
	     | null Instruction         = cfg
		 | acceptState              = cfg
		 | isStuck cfg              = cfg
		 | isStuck cfg              = cfg
		 | otherwise  s             = runFrom(doNextMove cfg)
		 where
		   isStuck cfg@(BATConfig b1 c) = null moves
		   moves =


maxBoxNum :: Program -> Int
maxBoxNum p =  maximum map(\x-> getIns x) p

getIns :: Instruction -> Int
getIns (INC x) = x
getIns (CLR x) = x
getIns (JEQ x y t) = maximum [x,y]


-- The configuration of a BATcomputer is given once
-- you know how many tokens are in each box, and
-- which instruction should be executed next
data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)
-}
  clrBox :: [Int] -> [Int] -> [Int]
  clrBox b n = b !! n take n-1 xs ++ [] drop n-1

  incBox :: [Int] -> [Int] -> [Int]
  incBox b x = b !! xs : []

  jeqBox :: [Int] -> [Int] -> Int -> Int
  jeqBox b1 b2 t = if (b1=b2) then t else n
  
-- PROBLEM 2. YOUR CODE HERE
-- --------------------------
--instance Show BATConfig where
 -- show BATConfig b c = show b ++<"boxes" ++> show <"c++">

--	-}

--	-}
    -- PROBLEM 7: getOutput    :: cfg -> Output
   --    getOutput s
