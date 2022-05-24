import Data.Char
import System.IO


-- Exercise 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]


-- Exercise 2
type Board = [Int]


putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))


putBoard' :: Board -> IO ()
putBoard' b = putBoardAux' 1 b


putBoardAux' :: Int -> Board -> IO ()
putBoardAux' _ []     = return ()
putBoardAux' n (x:xs) = do putRow n x
                           putBoardAux' (n+1) xs


-- Exercise 3
putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow row num | (row,num) <- zip [1..] xs]


-- Exercise 4
getInt :: IO Int
getInt = do n <- getLine
            return (read n :: Int)
                   


partial :: Int -> Int -> IO ()
partial current 0         = putStrLn ("The total is: " ++ show current)
partial current remaining = do x <- getInt
                               partial (current + x) (remaining -1)


adder :: IO ()
adder = do putStr "How many numbers?: "
           n <- getInt
           partial 0 n


-- Exercise 5
adder' :: IO ()
adder' = do putStr "How many numbers?: "
            n <- getInt
            ints <- sequence [getInt | _ <- [1..n]]
            putStrLn ("The total is: " ++ show (sum ints))


-- Exercise 6
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x


readLine :: IO String
readLine = readLine' ""


readLine' :: String -> IO String
readLine' xs = do x <- getCh
                  case x of
                        '\n' -> do putChar '\n'
                                   return xs
                        '\DEL' -> if null xs then readLine' ""
                                  else do putStr "\b \b"
                                          readLine' (init xs)
                        _ -> do putChar x
                                readLine' (xs ++ [x])




