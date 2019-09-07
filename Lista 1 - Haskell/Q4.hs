isReplica :: String -> Int -> Char -> Bool
isReplica str 0 _ = True
isReplica str n char = (str == aux n char)

aux :: Int -> Char -> String
aux 0 _ = ""
aux n char = char : aux (n-1) char
