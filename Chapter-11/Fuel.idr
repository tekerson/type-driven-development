public export
data Fuel = Dry | More (Lazy Fuel)

public export
partial
forever : Fuel
forever = More forever
