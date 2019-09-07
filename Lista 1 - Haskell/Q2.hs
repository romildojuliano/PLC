btoi :: String -> Int
btoi [] = 0
btoi (head:tail) = 2 ^ length tail * auxBool head + btoi tail

auxBool :: Char -> Int
auxBool '0' = 0
auxBool _ = 1 -- Qualquer coisa diferente de zero em binário é 1

