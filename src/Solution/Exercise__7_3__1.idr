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

Functor Expr where
    map func (Val val) = Val (func val)
    map func (Add lhs rhs) = Add (map func lhs) (map func rhs)
    map func (Sub lhs rhs) = Sub (map func lhs) (map func rhs)
    map func (Mul lhs rhs) = Mul (map func lhs) (map func rhs)
    map func (Div lhs rhs) = Div (map func lhs) (map func rhs)
    map func (Abs val) = Abs (map func val)
