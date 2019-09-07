metade :: [Int] -> ([Int], [Int])
metade lista = (take (length lista `div` 2) lista, drop (length lista `div`2) lista)