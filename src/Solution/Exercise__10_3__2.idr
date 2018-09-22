module Shape_abs

export
data Shape = Triangle Double Double | Rectangle Double Double | Circle Double

export
total triangle : Double -> Double -> Shape
triangle = Triangle

export
total rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
total circle : Double -> Shape
circle = Circle

------------------------------------------------------------------------------------------------------------------------

export
data ShapeView : Shape -> Type where
    STriangle : ShapeView (triangle base height)
    SRectangle : ShapeView (rectangle width height)
    SCircle : ShapeView (circle radius)

export
total shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle base height) = STriangle
shapeView (Rectangle width height) = SRectangle
shapeView (Circle radius) = SCircle

------------------------------------------------------------------------------------------------------------------------

total area : Shape -> Double
area s with (shapeView s)
    area (triangle base height) | STriangle = base * height / 2.0
    area (rectangle width height) | SRectangle = width * 2.0
    area (circle radius) | SCircle = radius * radius * pi
