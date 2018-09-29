import Control.Monad.State

%default total

data Tree a = Empty | Node (Tree a) a (Tree a)

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do (empties, nodes) <- get
                          put (empties + 1, nodes)
countEmptyNode (Node left _ right) = do (empties, nodes) <- get
                                        put (empties, nodes + 1)
                                        countEmptyNode left
                                        countEmptyNode right
