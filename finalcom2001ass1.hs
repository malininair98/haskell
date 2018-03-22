{-
     COM2001 Spring Assignment 1
     Haskell Template
     (c) 2018 Mike Stannett
     Email: m.stannett@sheffield.ac.uk
-}

-- If you would like to add your name and/or registration number
-- to this file, please do so here:
-- Malini Nair
-- Registration Number: 160153765
--

type Input  = Int
type Output = Int

-- A program is something that tells a computer how to
-- move from one configuration to another, how to
-- recognize when a configuration represents a valid
-- accept state, and so on.

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
--------------------------------------------------
-- CLR b        == empty box b
{- all of these need to be fixed and will be below
CLR :: [Int] -> []
  CLR x = delete x
-- INC b        == add a token to box b
-- the error associated with INC has to do with DataKinds
--therefore fix it later
INC :: (Eq b) => Int -> Instruction b -> Int
  INC = \b -> b + 1
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same
--                 number of tokens, jump to instruction t
JEQ :: [Int] -> [Int] -> Int
-- indents will be fixed later
  JEQ x y t =
            let filter(\x -> length x)
 	            filter(\y -> length y )
     	     in  [target]
-}

  clrBox :: [Int] -> [Int] -> [Int]
  clrBox b n = b !! n take n-1 xs ++ []

data Instruction
  = CLR {box :: Int}
  | INC {box :: Int}
  | JEQ {box1   :: Int,
         box2   :: Int,
         target :: Int}
  deriving (Eq, Show)

type Program = [Instruction]


-- PROBLEM 1. YOUR CODE HERE
-- --------------------------
-- Each instruction in a program refers to one or
-- more boxes.  What is the highest box number used
-- anywhere in the program?

{- this needs to be deleted 
maxBoxNum :: Program -> Int
  maxBoxNum CLR INC JEQ  =

concatList Program -> [Int]
  concatList CLR INC JEQ  = let (x:xs) = splitAt target xs in ys ++[target] ++ zs
					        in xs ++ ys ++ zs
-}

-- The configuration of a BATcomputer is given once
-- you know how many tokens are in each box, and
-- which instruction should be executed next
data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)


-- PROBLEM 2. YOUR CODE HERE
-- --------------------------
instance Show BATConfig where
--minimal complete definitons
  show BATConfig b c = show b ++<"boxes" ++> show <"c++">

-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.
instance ProgrammableComputer BATConfig  where
    -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg
	-- need to check the paramaters as in the BATconfig doesn't match the box
	-- number of params /= to the number args in the type definition
	-- needs to be fixed after the first problems have been completed
    initialise p x = BATConfig box1[] p x
    -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool
    acceptState  :: Program -> cfg -> Bool
	acceptState (BATConfig ProgrammableComputer)
	  = null BATConfig && null Instruction
    -- PROBLEM 5: doNextMove   :: Program -> cfg -> cfg
    doNextMove p cfg@(BATConfig b1 c)
	  | null Instruction      = cfg
      | acceptState cfg       = cfg
      | otherwise             = (BATConfig b1' c')
	  where
	       (BATConfig b1' c') = if (null b1') then (Program CLR JEQ INC)
		                      = else head n
									
									
			adjust b1 b2 c = case (CLR, INC, JEQ) of 
			  ([]  ,  _  , c+1)     -> clrBox
              (b1+1,  _  , c+1)		-> incBox
              (b1  ,  b2 , c+1)		-> jeqBox	
    -- PROBLEM 6: runFrom      :: Program -> cfg -> cfg
    runFrom p cfg@(BATConfig b1 c)
	  | acceptState              = cfg
      | otherwise                = runFrom(doNextMove cfg)
		
    -- PROBLEM 7: getOutput    :: cfg -> Output
    getOutput cfg@(BATConfig b1 c) out  
	  | acceptState cfg     = take out xs 
      | otherwise 	        = (BATConfig b1' c')
	 
	         
-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs
execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)


-- PROBLEM 8. YOUR CODE HERE
-- ---------------------------
-- start a program at instruction n instead of 0.  In other
-- words, change Jump instructions from (J x y t) to (J x y (t+n))
-- and leave all other instructions unchanged.
transpose :: Int -> Program -> Program
transpose



-- PROBLEM 9. YOUR CODE HERE
-- ---------------------------
-- join two programs together, so as to run one
-- after the other
(*->*) :: Program -> Program -> Program
p1 *->* p2 = ...


-- PROBLEM 10. YOUR CODE HERE
-- ---------------------------
-- program to compute B1 = B1 + B2
adder :: Program
adder = ...


-- PROBLEM 11. YOUR CODE HERE
-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)
copyBox :: Int -> Int -> Program
copyBox m n =


-- PROBLEM 12. YOUR CODE HERE
-- ---------------------------
-- program to compute B1 = Bx + By
addXY :: Int -> Int -> Program
addXY x y =
