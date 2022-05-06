{-
Exercise 7.9
A function that alternately applies ist two argument functions to succesive elements in a list, 
in turn about order.
-}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = map (\x -> (fst x) (snd x)) (zip ((concat . repeat) [f,g]) xs)


-- Doubles a digit and substracts 9 if the result is greater than 9
luhnDouble :: Int -> Int
luhnDouble n | n*2 <= 9 = n*2
             | otherwise = n*2 - 9

-- Decides if a card number of any length is vaid
luhn :: [Int] -> Bool
luhn xs = sum(altMap id luhnDouble (reverse xs)) `mod` 10 == 0