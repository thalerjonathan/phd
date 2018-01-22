data DoorState = DoorClosed | DoorOpen 

data DoorCmd : Type -> DoorState -> DoorState -> Type where
     Open     : DoorCmd () DoorClosed DoorOpen
     Close    : DoorCmd () DoorOpen   DoorClosed
     RingBell : DoorCmd () s s

     Pure     : t -> DoorCmd t s s
     (>>=)    : DoorCmd a s1 s2 -> (a -> DoorCmd b s2 s3) -> DoorCmd b s1 s3

doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do
  RingBell
  Open
  Close

doorProg' : DoorCmd () DoorClosed DoorClosed
doorProg' = do
  RingBell
  Open
  RingBell
  Close