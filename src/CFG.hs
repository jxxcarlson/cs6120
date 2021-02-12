module CFG where

import Data.List
import qualified Data.Map as Map 

data Instruction =   Instr { op :: Op, args :: [Value]} | Label String 
    deriving Show

data Op = Add | Sub | Mul | Div | Equal | Print | Branch | Jump deriving (Show, Eq)

data Value = I Int | B Bool | S String | R Int | Label_ String deriving Show

type BasicBlock = [Instruction]

type LabelToBlock = Map.Map String BasicBlock

type Edges = [(BasicBlock, BasicBlock)]    

type Nodes = [BasicBlock]

type CFG = ([BasicBlock], Edges)


-- PROGRAM

printProg :: [Instruction] -> IO ()
printProg is = putStrLn $ "\n" ++ prettyPrintProg is ++ "\n"


-- BLOCKS

getBlocks :: [Instruction] -> [BasicBlock]
getBlocks is = getBlocksAndMap is |> fst

printBlocks :: [BasicBlock] -> IO ()
printBlocks blocks = putStrLn $ "\nBlocks\n======" ++ prettyPrintBlocks blocks ++ "\n"

-- EDGES

getEdges :: [Instruction] -> Edges
getEdges is = 
    let 
        (bb, ltb) = getBlocksAndMap p1
    in
        formEdges (drop 1 bb) ltb

printEdges :: Edges -> IO ()
printEdges edges = 
    putStrLn $ "\n" ++ prettyPrintEdges edges ++ "\n"


-- MAP

getMap :: [Instruction] -> LabelToBlock
getMap is = getBlocksAndMap is  |> snd

printMap :: LabelToBlock -> IO ()
printMap ltb = 
    putStrLn $ prettyPrintLTB ltb 


-- MORE ...


emptyBlocks :: [BasicBlock]
emptyBlocks = []

getLabel_ :: Value -> String
getLabel_ v = 
    case v of 
        Label_ s -> s  
        _ -> ""

getLabel :: Instruction -> String
getLabel i = 
    case i of 
        Label s -> s  
        _ -> ""

getLabelsFromBlock :: BasicBlock ->  [String] 
getLabelsFromBlock bb = 
    case reverse bb of
        [] -> []
        (i:is)->
            case op i of 
                Jump -> map stringOfLabel_ (args i)   
                Branch -> map stringOfLabel_ (args i)   
                _ -> []

stringOfLabel_ :: Value  -> String
stringOfLabel_ v =
     case  v of 
          (Label_ s)  -> s
          _ -> ""
  
getLabelFromBlock :: BasicBlock -> Maybe String
getLabelFromBlock bb =
    case filter (\i -> isLabel i) bb of
        (i:is) -> 
            case i of 
                Label s -> Just s  
                _ -> Nothing
        [] -> Nothing

isLabel :: Instruction -> Bool 
isLabel i = 
    case i of 
        Label _ -> True 
        _ -> False





reorder :: ([BasicBlock], LabelToBlock) -> ([BasicBlock], LabelToBlock)
reorder (blocks, ltb) = 
    (reverse $ map reverse blocks, ltb)

getOutLabels :: Instruction -> [String]
getOutLabels i' = case op i' of
    Jump -> map getLabel_ $ args i'
    Branch -> map getLabel_ $ args i'
    _ -> []


getBlocksAndMap :: [Instruction] -> ([BasicBlock], LabelToBlock)
getBlocksAndMap is = reorder $ getBlocksAndMapHelper is [] (emptyBlocks, Map.empty )


getBlocksAndMapHelper :: [Instruction] ->  BasicBlock -> ([BasicBlock], LabelToBlock) -> ([BasicBlock], LabelToBlock)
getBlocksAndMapHelper [] currentBlock (blocks, ltb)= (currentBlock : blocks, postBlock currentBlock ltb)
getBlocksAndMapHelper (i:is) currentBlock (blocks, ltb) =
    let
        newBlock = i:currentBlock

        outLabels = getOutLabels i

    in
        if isTerminator i  then
            getBlocksAndMapHelper is [] (newBlock:blocks,  postBlock newBlock ltb )
        else if isLabel i then 
            getBlocksAndMapHelper is [i] (currentBlock:blocks,  postBlock currentBlock ltb)
        else 
            getBlocksAndMapHelper is newBlock (blocks, ltb)

postBlock b ltb =
    case getLabelFromBlock b of 
        Just l ->  Map.insert l (reverse b) ltb
        Nothing -> ltb

formEdges :: Nodes  -> LabelToBlock -> Edges
formEdges bbs ltb = formEdges_ bbs ltb []

formEdges_ :: Nodes -> LabelToBlock -> Edges -> Edges
formEdges_ [] ltb edges = edges
formEdges_ (b1:b2:bs) ltb edges = 
   let
       keys :: [String]
       keys = args (lastInstr b1) |> map getLabel_

       ee = map (\k -> Map.lookup k ltb) keys 
       
       ee'' = ee |> 
          fmap (\n -> (Just b1,n))

       mNewEdges = sequence $ map merge ee''
   in
   case mNewEdges of 
       Nothing -> formEdges_ (b2:bs) ltb (edges)
       Just newEdges -> formEdges_ (b2:bs) ltb (newEdges ++ edges)
formEdges_ (b1:bs) ltb edges = edges


merge :: (Maybe a, Maybe b) -> Maybe (a, b)
merge (u, v) = 
    case (u, v) of 
         (Just x, Just y) -> Just (x, y)
         _ -> Nothing

firstInstr :: BasicBlock -> Instruction
firstInstr (i:is) = i  

lastInstr :: BasicBlock -> Instruction
lastInstr = firstInstr . reverse
    


              

      
-- addEdges :: Edges -> String -> [String] -> Edges
-- addEdges es inStr outStrs = es
      

isTerminator :: Instruction -> Bool 
isTerminator i =
    case i of
        Instr op data_ -> op == Branch || op == Jump
        Label _ -> False


-- PRINTING


-- IO()-VALUED




printCFG :: CFG -> IO ()
printCFG (blocks, edges) = 
    do
        printBlocks blocks
        printEdges edges

-- STRING-VALUED

prettyPrintArgs :: [Value] -> String 
prettyPrintArgs vs = intercalate ", " $ map show vs


prettyPrintInstr :: Instruction -> String
prettyPrintInstr = prettyPrintInstr_

trimTrailing c = reverse . trimLeading c . reverse 


prettyPrintInstr_ :: Instruction -> String
prettyPrintInstr_ i = 
    case i of 
        Instr _ _ -> show (op i) ++ ": " ++ prettyPrintArgs (args i)
        Label s -> "." ++ s

prettyPrintBB :: BasicBlock -> String
prettyPrintBB bb = 
    intercalate "\n" $ map prettyPrintInstr bb

prettyPrintProg :: [Instruction] -> String
prettyPrintProg = prettyPrintBB

prettyPrintBlocks :: [BasicBlock] -> String
prettyPrintBlocks blocks =
    intercalate "\n\n" (map (prettyPrintBB . drop 0) blocks)


showPair :: (String, String) -> String
showPair (a,b) = "(" ++ a ++ ", " ++ b ++ ")"

showPairs :: [(String, String)] -> String
showPairs ps = intercalate "\n" (map showPair ps)

prettyPrintEdges :: Edges -> String
prettyPrintEdges edges = 
    intercalate "\n\n=================\n" (map prettyPrintEdge edges)

prettyPrintEdge :: (BasicBlock, BasicBlock) -> String
prettyPrintEdge (b1, b2) = 
        "node:\n-----\n" ++ intercalate "\n" [ prettyPrintBB b1, "--------", "successor" , "--------", prettyPrintBB b2]



prettyPrintLTB :: LabelToBlock -> String
prettyPrintLTB ltb = 
  let
    strs = map (\(l, bb) -> l ++ "\n-----\n" ++ prettyPrintBB bb)  (Map.toList ltb)
  in 
    "\n" ++ intercalate "\n\n" strs ++ "\n"
    


trimLeading :: Char -> String -> String
trimLeading c s = 
    case s of 
        (x:xs) -> if x == c then trimLeading c xs else s 
        [] -> ""

(|>) x f = f x

-- EXAMPLES
{-

printProg p1
printBlocks $ getBlocks p1

-}

p1 = [  Label "begin"
         , Instr {op = Add, args = [R 1, I 2, I 2]}
         , Instr {op = Sub, args = [R 2, R 1, I 1]}
         , Instr {op = Equal, args = [R 1, R 2]}
         , Instr {op = Branch, args = [Label_ "here", Label_ "there"]}
         , Instr {op = Mul, args = [R 3, R 1, R 2]}
         , Label "here"
         , Instr {op = Print, args = [S "A"]}
         , Label "there"
         , Instr { op = Print, args = [S "B"]}
        ]
