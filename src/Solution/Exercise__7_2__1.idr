data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs

Show ty => Show (Expr ty) where
    show (Val val) = show val
    show (Add lhs rhs) = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
    show (Sub lhs rhs) = "(" ++ show lhs ++ " - " ++ show rhs ++ ")"
    show (Mul lhs rhs) = "(" ++ show lhs ++ " * " ++ show rhs ++ ")"
    show (Div lhs rhs) = "(" ++ show lhs ++ " / " ++ show rhs ++ ")"
    show (Abs exp) = "(abs " ++ show exp ++ ")"
