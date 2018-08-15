data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

total insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left val right) = case compare x val of
    LT => Node (insert x left) val right
    EQ => Node left val right
    GT => Node left val (insert x right)

total listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

total treeToList : Tree a -> List a
treeToList tree = loop [] tree where
    loop : List a -> Tree a -> List a
    loop xs Empty = xs
    loop xs (Node left val right) = loop (val :: (loop xs right)) left
