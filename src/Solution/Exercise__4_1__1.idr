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
