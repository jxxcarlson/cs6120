module CFG where

import Data.List
import qualified Data.Map as Map 

data Instruction =   Instr { op :: Op, args :: [Value]} | Label String 
    deriving Show

data Op = Add | Sub | Mul | Div | Equal | Print | Branch | Jump deriving (Show, Eq)

data Value = I Int | B Bool | S String | R Int | Label_ String deriving Show

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
printBlocks blocks = putStrLn $ "\n" ++ prettyPrintBlocks blocks ++ "\n"


type BasicBlock = [Instruction]

type Edges = Map.Map String String

getLabel :: BasicBlock -> String
getLabel bb =
    case head $ filter (\i -> isLabel i) bb of
        Label s -> s  
        _ -> "X"

isLabel :: Instruction -> Bool 
isLabel i = 
    case i of 
        Label _ -> True 
        _ -> False

getBlocks :: [Instruction] -> [BasicBlock]
getBlocks is = reverse $ map reverse $ getBlocks_ is [] []


getBlocks_ :: [Instruction] ->  BasicBlock -> [BasicBlock] -> [BasicBlock] 
getBlocks_ [] currentBlock blocks  = currentBlock : blocks
getBlocks_ (i:is) currentBlock blocks =
    let
        newBlock = i:currentBlock
    in
        if isTerminator i then
            getBlocks_ is [] (newBlock:blocks)
        else 
            getBlocks_ is newBlock blocks


isTerminator :: Instruction -> Bool 
isTerminator i =
    case i of
        Instr op data_ -> op == Branch || op == Jump
        Label _ -> False

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