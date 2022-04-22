-- Exercise 2.4
n = a `div` length xs
    where
        a = 10
        xs =[1,2,3,4,5]

-- Exercise 2.5
init' xs = take (length xs - 1) xs

init'' xs = reverse (tail (reverse xs))