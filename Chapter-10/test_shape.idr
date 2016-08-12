import Shape_abs

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = (base * height) / 2
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = 2 * pi * radius * radius
