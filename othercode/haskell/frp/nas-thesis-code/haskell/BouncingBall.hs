module BouncingBall where

------------------------------------------------

type Acceleration = Double
type Velocity = Double
type Height = Double

type Ball = (Height , Velocity)

g :: Acceleration
g = 9.81

detectImpact :: Ball -> Bool
detectImpact (h , v) = h <= 0

negateVel :: Ball -> Ball
negateVel (h , v) = (h , negate v)

------------------------------------------------
