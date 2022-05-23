{-
An action that reads three characters,
discards the second, and returns the first and third as pair.
-}
act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)


{-
An action that prompts for a string to be entered from the keyboard,
and displays its length.
-}
strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"