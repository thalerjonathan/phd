data DoorState = DoorClosed | DoorOpen 
data DoorResult = OK | Jammed

data DoorCmd : (t : Type) -> DoorState -> (t -> DoorState) -> Type where
     Open     : DoorCmd DoorResult DoorClosed (\res => case res of
                                                            OK => DoorOpen
                                                            Jammed => DoorClosed)
     Close    : DoorCmd () DoorOpen (const DoorClosed)
     RingBell : DoorCmd () DoorClosed (const DoorClosed)

     Display  : String -> DoorCmd () s (const s)

     Pure     : t -> DoorCmd t (state_fn s) state_fn
     (>>=)    : DoorCmd a state1 state2_fn -> 
                ((res : a) -> DoorCmd b (state2_fn res) state3_fn) 
                -> DoorCmd b state1 state3_fn

doorProg : DoorCmd () DoorClosed (const DoorClosed)
doorProg = do
  RingBell
  jam <- Open
  case jam of
       OK     => do
         Display "Glad To Be Of Service"
         Close
       Jammed => Display "Door Jammed"

doorProgPattern : DoorCmd () DoorClosed (const DoorClosed)
doorProgPattern = do
  RingBell
  OK <- Open | Jammed => Display "Door Jammed"
  Display "Glad To Be Of Service"
  Close

doorProgPatternTwice : DoorCmd () DoorClosed (const DoorClosed)
doorProgPatternTwice = do
  RingBell
  OK <- Open | Jammed => Display "Door Jammed"
  Display "Glad To Be Of Service"
  Close
  OK <- Open | Jammed => Display "Door Jammed"
  Display "Glad To Be Of Service"
  Close