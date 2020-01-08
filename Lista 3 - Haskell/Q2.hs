data Tree t = Nilt | Node t (Tree t) (Tree t)
    deriving Show

isBST :: (Ord t) => Tree t -> Bool
isBST Nilt = True
isBST (Node val left right) = isBST(left) && isBST(right) && (compare val left right)
    where compare val Nilt Nilt = True
          compare val (Node leftval _ _) Nilt = (leftval < val)
          compare val Nilt (Node rightval _ _) = (rightval > val)
          compare val (Node leftval _ _) (Node rightval _ _) =  (leftval < val && rightval > val)

