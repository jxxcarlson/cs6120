module CFG where

import Data.List
import qualified Data.Map as Map 

data Instruction =   Instr { op :: Op, args :: [Value]} | Label String 
    deriving Show

data Op = Add | Sub | Mul | Div | Equal | Print | Branch | Jump deriving (Show, Eq)

data Value = I Int | B Bool | S String | R Int | Label_ String deriving Show

type BasicBlock = [Instruction]

type Edges = [(String, String)]
           

type CFG = ([BasicBlock], Edges)

emptyBlocks :: [BasicBlock]
emptyBlocks = []

getLabel_ :: Value -> String
getLabel_ v = 
    case v of 
        Label_ s -> s  
        _ -> ""

getLabel :: BasicBlock -> String
getLabel bb =
    case filter (\i -> isLabel i) bb of
        (i:is) -> 
            case i of 
                Label s -> s  
                _ -> "X"
        [] -> "nada"

isLabel :: Instruction -> Bool 
isLabel i = 
    case i of 
        Label _ -> True 
        _ -> False

getCFG :: [Instruction] -> ([BasicBlock], Edges)
getCFG is = fixup $ getCFG_ is [] (emptyBlocks, [])

fixup :: CFG -> CFG
fixup (blocks, edges) = 
    (reverse $ map reverse blocks, reverse edges)

getOutLabels :: Instruction -> [String]
getOutLabels i' = case op i' of
    Jump -> map getLabel_ $ args i'
    Branch -> map getLabel_ $ args i'
    _ -> []

getCFG_ :: [Instruction] ->  BasicBlock -> CFG -> CFG
getCFG_ [] currentBlock (blocks, edges)= (currentBlock : blocks, edges)
getCFG_ (i:is) currentBlock (blocks, edges) =
    let
        newBlock = i:currentBlock

        outLabels = getOutLabels i

        inLabel = getLabel newBlock

    in
        if isTerminator i  then
            getCFG_ is [] (newBlock:blocks,  addEdges edges inLabel outLabels)
        else if isLabel i then 
            getCFG_ is [i] (currentBlock:blocks,  edges)
        else 
            getCFG_ is newBlock (blocks, edges)


accumulator_ :: String -> String -> Map.Map String String -> Map.Map String String  
accumulator_ val key = Map.insert key val     

addEdges :: Edges -> String -> [String] -> Edges
addEdges es inStr outStrs =
    Prelude.foldr accumulator es outStrs
      where accumulator out m = (inStr, out):m
      
-- addEdges :: Edges -> String -> [String] -> Edges
-- addEdges es inStr outStrs = es
      

isTerminator :: Instruction -> Bool 
isTerminator i =
    case i of
        Instr op data_ -> op == Branch || op == Jump
        Label _ -> False


-- PRINTING

prettyPrintArgs :: [Value] -> String 
prettyPrintArgs vs = intercalate ", " $ map show vs

prettyPrintInstr :: Instruction -> String
prettyPrintInstr i = 
    case i of 
        Instr _ _ -> show (op i) ++ ": " ++ prettyPrintArgs (args i)
        Label s -> "." ++ s

prettyPrintProg :: [Instruction] -> String
prettyPrintProg is = intercalate "\n" (map prettyPrintInstr is)

prettyPrintBlocks :: [BasicBlock] -> String
prettyPrintBlocks blocks =
    intercalate "\n\n" (map prettyPrintProg blocks)

printProg :: [Instruction] -> IO ()
printProg is = putStrLn $ "\n" ++ prettyPrintProg is ++ "\n"


printBlocks :: [BasicBlock] -> IO ()
printBlocks blocks = putStrLn $ "\nBlocks\n======\n" ++ prettyPrintBlocks blocks ++ "\n"


showPair :: (String, String) -> String
showPair (a,b) = "(" ++ a ++ ", " ++ b ++ ")"

showPairs :: [(String, String)] -> String
showPairs ps = intercalate "\n" (map showPair ps)

printEdges :: Edges -> IO ()
printEdges es = 
    let 

        edgeListAsString :: String
        edgeListAsString = showPairs es
    in
    putStrLn $ "Edges\n=====\n" ++ edgeListAsString ++ "\n"
  
printCFG :: CFG -> IO ()
printCFG (blocks, edges) = 
    do
        printBlocks blocks
        printEdges edges

-- EXAMPLES
prog1 = [  Label "begin"
         , Instr {op = Add, args = [R 1, I 2, I 2]}
         , Instr {op = Sub, args = [R 2, R 1, I 1]}
         , Instr {op = Equal, args = [R 1, R 2]}
         , Instr {op = Branch, args = [Label_ "here", Label_ "there"]}
         , Instr {op = Mul, args = [R 3, R 1, R 2]}
         , Label "here"
         , Instr {op = Print, args = [S "A"]}
         , Instr {op = Jump, args = [Label_ "end"]}
         , Label "there"
         , Instr { op = Print, args = [S "B"]}
         , Label "end"
        ]