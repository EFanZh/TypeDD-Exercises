data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr

total evaluate : Expr -> Int
evaluate (Val num) = num
evaluate (Add lhs rhs) = (evaluate lhs) + (evaluate rhs)
evaluate (Sub lhs rhs) = (evaluate lhs) - (evaluate rhs)
evaluate (Mult lhs rhs) = (evaluate lhs) * (evaluate rhs)
