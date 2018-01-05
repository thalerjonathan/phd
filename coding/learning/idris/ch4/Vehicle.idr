data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Tram : (charge : Nat) -> Vehicle Electric
     ECar : (charge : Nat) -> Vehicle Electric

total
wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Tram charge) = 12
wheels (ECar charge) = 4

-- could return another vehicle here, i guess Idris allows
-- to capture this invariante also at type-level but I dont know enough about it yet
total
refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50 
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
refuel Unicycle impossible

total
charge : Vehicle Electric -> Vehicle Electric
charge (Tram charge) = Tram 5000
charge (ECar charge) = ECar 1000
