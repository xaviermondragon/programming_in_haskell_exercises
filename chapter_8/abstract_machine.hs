data Expr = Val Int | Add Expr Expr | Mult Expr Expr


type Cont = [Op]


data Op = EVAL Expr | ADD Int | MULT Int


eval :: Expr -> Cont -> Int
eval (Val n)   c  = exec c n
--eval (Add x y) c = eval x (EVAL y : c) 
eval (Add x y) c  = eval x ((ADD (eval y []) ) : c) 
eval (Mult x y) c = eval x ((MULT (eval y []) ) : c)

exec :: Cont -> Int -> Int
exec [] n           = n
--exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m  = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)


{-
First version of value
value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y
-}
value :: Expr -> Int
value e = eval e []


{-
Expressions to test with
value (Val 2)
value (Add (Val 2) (Val 3))
value (Add (Add (Val 2) (Val 3)) (Val 4))
value (Mult (Val 2) (Val 3))
-}
 