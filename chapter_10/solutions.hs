-- Exercise 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]


-- Exercise 2 and 3
type Board = [Int]


putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))


putBoard' :: Board -> IO ()
putBoard' xs = auxFunc (zip [1..] xs)
               where auxFunc [] = return ()
                     auxFunc (x:xs) = do putRow (fst x) (snd x)
                                         auxFunc (xs)


putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow row num | (row,num) <- zip [1..] xs]