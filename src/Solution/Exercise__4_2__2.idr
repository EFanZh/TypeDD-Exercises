data PowerSource = Petrol | Pedal | Eletric

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol
    Tram : Vehicle Eletric
    EletricCar : Vehicle Eletric
