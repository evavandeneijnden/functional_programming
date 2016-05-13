module FP_Core where
  import FPPrac.Trees
{-
Extension of CoreIntro.hs:
- instructions as program *in* the processor,
- stack now is list of fixed length,i
- clock added
- program counter + stack pointer added,
- instruction EndProg added,
- update operaton (<~) added,
-}

-- ========================================================================

type Stack  = [Int]

type Heap = [Int]

data Op     = Add | Mul | Sub
            deriving Show

data Instr  = PushConst Int
            | Calc Op
            | PushAddr Int
            | Store Int
            | EndProg
            deriving Show

data Tick = Tick

data Expr = Const Int                   -- for constants
          | Variable String
          | BinExpr Op Expr Expr        -- for ``binary expressions''

data Statement = Assign Variable Expr

data LookupTuples = [(String, Int)]

lookup :: String -> LookupTuples -> Int
lookup varname [] = error "Unknown variable"
lookup varname (x:xs) | fst x == varname = snd x
                      | otherwise = lookup varname xs
-- ========================================================================
-- Processor functions

xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)


core :: [Instr] -> (Int,Int,Heap,Stack) -> Tick -> (Int,Int,Heap,Stack)

core instrs (pc,sp,heap,stack) tick =  case instrs!!pc of

        PushConst n   -> (pc+1, sp+1 , heap, stack <~ (sp,n))

        Calc op  -> (pc+1, sp-1 , heap, stack <~ (sp-2,v))
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        PushAddr n -> (pc+1, sp+1, heap, stack <~ (sp, (heap !! n)))

        Store n -> (pc+1, sp?, ys ++ [last stack] ++ zs, init stack)
                where
                  (ys,zs) = splitAt n heap

        EndProg  -> (-1, sp, heap, stack)

codeGen :: Expr -> [Instr]
codeGen expr = instrGen expr ++ [EndProg]

instrGen :: Expr -> [Instr]
instrGen (Const x) = [Push x]
instrGen (BinExpr op expr1 expr2) = (instrGen expr1) ++ (instrGen expr2) ++ [Calc op]

codeGen' :: Expr -> [Instr]
codeGen' expr = instrGen' expr ++ [EndProg]

instrGen' :: Expr -> [Instr]
instrGen' (Const x) = [PushConst x]
instrGen' (Variable x) = [PushAddr (lookup x lookuptable)]
instrGen' (BinExpr op expr1 expr2) = (instrGen expr1) ++ (instrGen expr2) ++ [Calc op]

-- ========================== Tree stuff ==================================

data BinTree a b  = BinLeaf b
                | BinNode a (BinTree a b) (BinTree a b)

ppBinTree :: (Show a, Show b) => (BinTree a b) -> RoseTree
ppBinTree (BinLeaf b) = RoseNode (show b) []
ppBinTree (BinNode a t1 t2) = RoseNode (show a) [ppBinTree t1, ppBinTree t2]

type ExprTree = BinTree Op Int

exprToExprTree :: Expr -> ExprTree
exprToExprTree (Const x) = BinLeaf (Const x)
exprToExpreTree (Variable x) = BinLeaf (Variable x)
exprToExprTree (BinExpr op expr1 expr2) = BinNode op (exprToExprTree expr1) (exprToExprTree expr2)


-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr = BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))

-- The program that results in the value of the expression (1105):
prog = [ Push 2
       , Push 10
       , Calc Mul
       , Push 3
       , Push 4
       , Push 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , Push 12
       , Push 5
       , Calc Add
       , Calc Mul
       , EndProg
       ]

-- Testing
clock      = repeat Tick
emptyStack = replicate 8 0
test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_) -> pc /= -1)

           $ scanl (core prog) (0,0,emptyStack) clock
