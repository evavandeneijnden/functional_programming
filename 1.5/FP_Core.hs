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

  type Heap   = [Int]

  data Op     = Add | Mul | Sub
              deriving Show

  data Instr  = PushConst Int
              | Calc Op
              | PushAddr Int
              | Store Int
              | PushPC
              | EndRep
              | EndProg
              deriving Show

  data Tick = Tick
              deriving Show

  data Expr = Const Int                   -- for constants
            | Variable Int
            | BinExpr Op Expr Expr        -- for ``binary expressions''

  data Statement = Assign Int Expr
                |  Repeat Expr [Statement]

  class CodeGen se where
    codeGen' :: se -> [Instr]

  instance CodeGen Expr where
    codeGen' expr = instrGen expr

  instance CodeGen Statement where
    codeGen' stat = instrGen' stat

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

          Store n -> (pc+1, sp-1, heap <~ (n, (stack !! (sp-1))), init stack)

          PushPC -> (pc+1, sp+1, heap, stack ++ [pc+1])

          EndRep -> if ((stack !! (sp-2)) == 0) then (pc+1, sp, heap, init (init stack))
                    else ((last stack), sp, heap, stack <~ ((sp-2), (stack !! (sp-2))-1))

          EndProg  -> (-1, sp, heap, stack)

  compile :: Statement -> [Instr]
  compile stat = codeGen' stat ++ [EndProg]

  instrGen :: Expr -> [Instr]
  instrGen (Const x) = [PushConst x]
  instrGen (Variable x) = [PushAddr x]
  instrGen (BinExpr op expr1 expr2) = (codeGen' expr1) ++ (codeGen' expr2) ++ [Calc op]

  instrGen' :: Statement -> [Instr]
  instrGen' (Assign addr expr) = (codeGen' expr) ++ [Store addr]
  instrGen' (Repeat expr stats) = (codeGen' expr) ++ [PushPC] ++ (foldl (++) [] (map codeGen' stats)) ++ [EndRep]

  -- ========================== Tree stuff ==================================

  data BinTree a b  = BinLeaf b
                  | BinNode a (BinTree a b) (BinTree a b)

  ppBinTree :: (Show a, Show b) => (BinTree a b) -> RoseTree
  ppBinTree (BinLeaf b) = RoseNode (show b) []
  ppBinTree (BinNode a t1 t2) = RoseNode (show a) [ppBinTree t1, ppBinTree t2]

  type ExprTree = BinTree Op Expr

  exprToExprTree :: Expr -> ExprTree
  exprToExprTree (Const x) = BinLeaf (Const x)
  exprToExprTree (Variable x) = BinLeaf (Variable x)
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

  stat = Repeat (Const 3)
          [(Assign 2
              (Const 36)),
           (Assign 3
              (Variable 2))
          ]

  -- Testing
  clock      = repeat Tick
  emptyHeap :: [Int]
  emptyHeap = replicate 8 0

  -- -- The program that results in the value of the expression (1105):
  -- prog = [ PushConst 2
  --        , PushConst 10
  --        , Calc Mul
  --        , PushConst 3
  --        , PushConst 4
  --        , PushConst 11
  --        , Calc Add
  --        , Calc Mul
  --        , Calc Add
  --        , PushConst 12
  --        , PushConst 5
  --        , Calc Add
  --        , Calc Mul
  --        , EndProg
  --        ]
  --
  -- -- Testing
  -- clock      = repeat Tick
  -- emptyStack = replicate 8 0
  -- test       = putStr
  --            $ unlines
  --            $ map show
  --            $ takeWhile (\(pc,_,_) -> pc /= -1)
  --
  --            $ scanl (core prog) (0,0,emptyStack) clock
