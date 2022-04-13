halve :: [a] -> ([a], [a])
halve xs  = (take n xs, drop n xs)
            where n = length xs `div` 2

halve' :: [a] -> ([a], [a])
halve' xs  = splitAt (length xs `div` 2) xs