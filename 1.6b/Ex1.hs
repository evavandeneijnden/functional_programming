data Expr = Const Int
        | Boolean Bool
        | Var String
        | Tuple2 Expr Expr
        | Tuple3 Expr Expr Expr
        | BinOp String Expr Expr
        | ITE Expr Expr Expr
        | App Expr Expr
        | Lambda String Type Expr
data Type = IntType
        | BoolType
        | FunType Type Type
        | DoubleType Type Type
        | TripleType Type Type Type
    deriving (Eq, Show)

type Env = [(String, Type)]

env = [("+", FunType IntType (FunType IntType IntType)),
    ("-", FunType IntType (FunType IntType IntType)),
    ("*", FunType IntType (FunType IntType IntType)),
    ("&&", FunType BoolType (FunType BoolType BoolType)),
    ("||", FunType BoolType (FunType BoolType BoolType)),
    ("x", IntType),
    ("y", IntType),
    ("true", BoolType),
    ("false", BoolType),
    ("2,3", DoubleType IntType IntType),
    ("true,6", DoubleType BoolType IntType),
    ("3,true,6", TripleType IntType BoolType IntType)]

typeOf :: Env -> Expr -> Type

typeOf env (BinOp op e1 e2) = case top of
                            Nothing -> error "Type of operator not found"
                            Just (FunType t0 (FunType t1 t2))
                                | t0 == te1 && t1 == te2 -> t2
                            Just _ -> error "Type mismatched for operator"
                        where
                            top = lookup op env
                            te1 = typeOf env e1
                            te2 = typeOf env e2

typeOf env (Var x) = case t of
                    Nothing -> error "Type of variable not found"
                    Just someType -> someType
                where
                    t = lookup x env

typeOf env (Const _) = IntType
typeOf env (Boolean _) = BoolType

typeOf env (Tuple2 ex1 ex2) = DoubleType (typeOf env ex1) (typeOf env ex2)

typeOf env (Tuple3 e1 e2 e3) = TripleType (typeOf env e1) (typeOf env e2) (typeOf env e3)

typeOf env (Lambda x a expr) = FunType a (typeOf env2 expr)
                            where
                                env2 = env ++ [(x,a)]

typeOf env (ITE clause e1 e2) = case clauseT of
                                BoolType
                                    | te1 == te2 -> te1
                                    | otherwise -> error "Then and else different types"
                                _ -> error "Type of if-then-else-clause not found"
                            where
                                clauseT = typeOf env clause
                                te1 = typeOf env e1
                                te2 = typeOf env e2

typeOf env (App f x) = case tf of
                    FunType t0 t1
                        | t0 == tx -> t1
                    _ -> error "Function is not a function"
                where
                    tx = typeOf env x
                    tf = typeOf env f
