luhnDouble :: Int -> Int
luhnDouble n | n*2 <= 9 = n*2
             | otherwise = n*2 - 9


luhn :: Int -> Int -> Int -> Int -> Bool
luhn k l n m | ((luhnDouble k) + l + (luhnDouble n) + m) `mod` 10 == 0 = True
             | otherwise = False