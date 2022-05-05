-- Exercise 7.2
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)
-- Book solution:
-- all p = and . map p


any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)
-- Book solution:
-- all p = or . map p


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x == True = x:(takeWhile' p xs)
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x == True = dropWhile' p xs
                    | otherwise = xs



-- Exercise 7.3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []
-- Book's solution:
-- map f = foldr (\x xs -> f x : xs) []


filter' :: (a -> Bool) -> ([a] -> [a])
filter' p = foldr ((++) . decision) []
          where decision x = if p x then [x] else []
-- Book's solution:
-- map f = foldr (\x xs -> if p x then x:xs else xs) []

-- Exercise 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\v x -> 10*v + x) 0

-- Exercise 7.5
curry' :: ((a,b) -> c) -> (a -> (b -> c))
curry' f = g
         where g x = h
                   where h y = f (x, y)
-- Book's solution
-- curry :: ((a,b) -> c) -> (a -> b -> c)
-- curry f = \x y -> f(x,y)


uncurry' :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry' f = g
           where g (x,y) = (f x) y
-- Book's solution
-- uncurry :: (a -> b -> c) -> ((a,b) -> c)
-- uncurry f = \(x,y) -> f x y


-- Exercise 7.6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)


chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8) 


map'' :: Eq a => (a -> b) -> [a] -> [b]
map'' f = unfold (== []) (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f x = unfold (\x -> False) id f x