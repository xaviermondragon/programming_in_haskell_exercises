import Data.List
main :: IO ()
main = print (nearestSolutions [1,3,7,10,25,50] 831)


-- Arithmetic operators
data Op = Add | Sub | Mul | Div | Exp 
          --deriving (Eq, Ord)


instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"


valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = x > 0 && y > 0


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y


-- Numeric expressions
data Expr = Val Int | App Op Expr Expr 
            --deriving (Eq, Ord)


instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                           brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"


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


-- Formalising the problem:
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = isChoice (values e) ns && eval e == [n]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = (elem x ys) && (isChoice xs (rmFstOcur x ys))


rmFstOcur :: Eq a => a -> [a] -> [a]
rmFstOcur _ []                 = []
rmFstOcur x (y:ys) | x == y    = ys
                   | otherwise = y : rmFstOcur x ys


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
ops = [Add,Sub,Mul,Div,Exp]


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
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]


{--
nearestSolutions' :: [Int] ->0 Int -> [Expr]
nearestSolutions' ns n =
  map snd (takeWhile (\x -> fst x == first) allResults)
  where
    allResults = sort [(abs (m - n), e) | ns' <- choices ns, (e, m) <- results ns']
    first = (fst . head) allResults
--}


nearestSolutions :: [Int] -> Int -> [Expr]
nearestSolutions ns n = [e | (e,n) <- xs, n == bestAprox]
                     where xs = [(e, abs(n-m)) | ns' <- choices ns, (e, m) <- results ns']
                           bestAprox = head (sort (map (\x -> snd x) xs))