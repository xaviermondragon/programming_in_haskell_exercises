main :: IO ()
--main = print (solutions [1,3,7,10,25,50] 765)
main = print (solutions' [1,3,7,10,25,50] 765)


-- Arithmetic operators
data Op = Add | Sub | Mul | Div


instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


-- Numeric expressions
data Expr = Val Int | App Op Expr Expr


instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                           brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"
{-
To test:
show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
-}


values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r


eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]


-- Combinatorial functions
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss 
              where yss = subs xs


interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


choices :: [a] -> [[a]]
choices = concat . map perms . subs
{-
To test:
choices [1,2,3]
-}

-- Formalising the problem:
solution :: Expr -> [Int] -> Int -> Bool
--solution e ns n = elem (values e) (choices ns) && eval e == [n]
solution e ns n = isChoice (values e) ns && eval e == [n]
{-
To test:
-}
e :: Expr
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))


-- Brute force solution:
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]


exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]


combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]


ops :: [Op]
ops = [Add,Sub,Mul,Div]


solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]


-- Combining generation and evaluation
type Result = (Expr,Int)


results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls,rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]


combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]


solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]


-- Exercise 9.1
choices' :: [a] -> [[a]]
choices' xs = [zs |ys <- subs xs, zs <- perms ys]


-- Exercise 9.2
rmFstOcur :: Eq a => a -> [a] -> [a]
rmFstOcur _ []                 = []
rmFstOcur x (y:ys) | x == y    = ys
                   | otherwise = y : rmFstOcur x ys


isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = (elem x ys) && (isChoice xs (rmFstOcur x ys))


{-
Exercise 9.4
length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns']
length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns', eval e /= []]
-}


{-
Exercise 9.5
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0 
length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns', eval e /= []]
-}