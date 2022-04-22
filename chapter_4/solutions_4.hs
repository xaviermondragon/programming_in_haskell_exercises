-- Exercise 4.1
halve :: [a] -> ([a], [a])
halve xs  = (take n xs, drop n xs)
            where n = length xs `div` 2

halve' :: [a] -> ([a], [a])
halve' xs  = splitAt (length xs `div` 2) xs

-- Exercise 4.2
third :: [a] -> a
third xs = head (tail (tail xs))



third' :: [a] -> a
third' xs = xs !! 2



third'' :: [a] -> a
third'' (_:_:x:xs) = x

-- Exercise 4.3
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs



safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs



safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- Exercise 4.4
(|||) :: Bool -> Bool -> Bool
False ||| False = False
False ||| True  = True
True  ||| False = True
True  ||| True  = True



(||||) :: Bool -> Bool -> Bool
False |||| False = False
_     |||| _     = True



(|||||) :: Bool -> Bool -> Bool
False ||||| b = b
True  ||||| _ = True

(||||||) :: Bool -> Bool -> Bool
b |||||| c | b==c = b
           | otherwise = True

-- Exercise 4.5
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a then
             if b then True
             else False
          else False

-- Exercise 4.6
(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a then b
           else False

-- Exercise 4.7
mult :: Int ->(Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x*y*z))