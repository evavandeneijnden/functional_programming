data Expr = Const Int
        | Var String
        | BinOp String Expr Expr
        | App Expr Expr
data Type = IntType
        | FunType Type Type
    deriving (Eq, Show)

type Env = [(String, Type)]

env = [("+", FunType IntType (FunType IntType IntType)),
    ("-", FunType IntType (FunType IntType IntType)),
    ("*", FunType IntType (FunType IntType IntType)),
    ("x", IntType),
    ("y", IntType)]

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

typeOf env (App f x) = case tf of
                    FunType t0 t1
                        | t0 == tx -> t1
                    _ -> error "Function is not a function"
                where
                    tx = typeOf env x
                    tf = typeOf env f
