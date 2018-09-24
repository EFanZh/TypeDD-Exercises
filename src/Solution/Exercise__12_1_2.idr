import Control.Monad.State

%default total

data Tree a = Empty | Node (Tree a) a (Tree a)

countEmpty : Tree a -> State Nat ()
countEmpty Empty = do count <- get
                      put (S count)
countEmpty (Node left _ right) = do countEmpty left
                                    countEmpty right
