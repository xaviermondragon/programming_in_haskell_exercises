-- Exercise 6.1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

-- Exercise 6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

-- Exercise 6.3
(^|) :: Int -> Int -> Int
_ ^| 0 = 1
m ^| n | n > 0 = m * (m ^| (n-1))

-- Exercise 6.4
euclid :: Int -> Int -> Int
euclid n m | n == m = n
           | otherwise = euclid (big -small) small
           where
               big   = max n m
               small = min n m

-- Exercise 6.6
and' :: [Bool] -> Bool
and' [x,y] = x && y
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x:xs) = x ++ (concat' xs)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x | n > 0 = x : (replicate' (n-1) x)

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) | y == x = True
               | otherwise = elem' y xs

-- Exercise 6.7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

-- Exercise 6.8
halve :: [a] -> ([a],[a])
halve xs = ((take n xs),(drop n xs))
         where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
         where ys = fst (pair)
               zs = snd (pair)
               pair = halve xs

-- Exercise 6.9
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) | n > 0 = x:(take' (n-1) xs)

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs