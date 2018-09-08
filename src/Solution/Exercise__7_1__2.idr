data Shape = Triangle Double Double | Rectangle Double Double | Circle Double

Eq Shape where
    (==) (Triangle base1 height1) (Triangle base2 height2) = base1 == base2 && height1 == height2
    (==) (Triangle length1 height1) (Triangle length2 height2) = length1 == length2 && height1 == height2
    (==) (Circle radius1) (Circle radius2) = radius1 == radius2
    (==) _ _ = False

total area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Ord Shape where
    compare lhs rhs = compare (area lhs) (area rhs)
