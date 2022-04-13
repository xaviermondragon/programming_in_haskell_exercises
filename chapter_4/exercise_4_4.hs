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