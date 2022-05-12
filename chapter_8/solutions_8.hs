-- Exercise 1
data Nat = Zero | Succ Nat


nat2Int :: Nat -> Int
nat2Int Zero     = 0
nat2Int (Succ n) = 1 + nat2Int n


int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n-1))


add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)


mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n)    = add m (mult m n)


-- Exercise 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)


occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                         = compare x y == EQ
occurs' x (Node l y r) | compare x y == EQ = True
                       | compare x y == LT = occurs' x l
                       | otherwise         = occurs' x r

{-
Book's solution:
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                 = x == y
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r
-}


t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- Exercise 3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)


numLeaves :: Tree' a -> Int
numLeaves (Leaf' _)     = 1
numLeaves (Node' l r) = (numLeaves l) + (numLeaves r)


balanced :: Tree' a -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = abs ((numLeaves l) - (numLeaves r)) <= 1
                     && balanced l && balanced r


t' :: Tree' Int
t' = Node' (Node' (Leaf' 1) (Leaf' 4)) (Node' (Leaf' 6) (Leaf' 9))


-- Exercise 4
splitList :: [a] -> ([a],[a])
splitList xs = (take n xs, drop n xs)
             where n = (length xs) `div` 2


balance :: [a] -> Tree a
balance [x]    = Leaf x
balance (x:xs) = Node (balance l) x (balance r)
             where l = fst (splitList xs)
                   r = snd (splitList xs)


{-
Book's solution:
halve xs = splitAt (length xs `div` 2) xs

balance [x]    = Leaf x
balance (x:xs) = Node (balance xs) (balance ys)
             where (ys, zs) = halve xs
-}


-- Exercise 7
instance Eq a => Eq (Maybe a) where
    Nothing == Nothing  = True
    Just x  == Just x   = True
    _       == _        = False


instance Eq a => Eq [a] where
    xs = ys | length xs /= length ys = False
            | otherwise = and [x == y | (x,y) <- zip xs ys]