total every_other : Stream a -> Stream a
every_other (x0 :: (x1 :: xs)) = x1 :: every_other xs
