data Ops = SUM | MUL | SUB
    deriving (Show, Eq)

data IntTree = Nilt Int | Node Ops IntTree IntTree
    deriving Show

evalTree :: IntTree -> Int
evalTree (Nilt n) = n
evalTree (Node SUM left right) = evalTree (left) + evalTree (right)
evalTree (Node SUB left right) = evalTree (left) - evalTree (right)
evalTree (Node MUL left right) = evalTree (left) * evalTree (right)
