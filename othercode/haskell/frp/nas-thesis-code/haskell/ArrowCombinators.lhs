This file contains some useful Arrow combinators

> module ArrowCombinators where

> import Prelude hiding (fst,snd)
> import Control.Arrow

-------------------------------------------------

> identity        :: Arrow a => a b b
> identity        =  returnA

> fst             :: Arrow a => a (b,c) b
> fst             =  arr (\(b,_) -> b)

> snd             :: Arrow a => a (b,c) c
> snd             =  arr (\(_,c) -> c)

> toFst           :: Arrow a => a b c -> a (b,x) c
> toFst f         =  fst >>> f

> toSnd           :: Arrow a => a b c -> a (x,b) c
> toSnd f         =  snd >>> f

> dup             :: Arrow a => a b (b,b)
> dup             =  identity &&& identity

> fork            :: Arrow a => a b (b,b)
> fork            =  dup

> swap            :: Arrow a => a (b,c) (c,b)
> swap            =  snd &&& fst

> forkFirst       :: Arrow a => a b c -> a b (c,b)
> forkFirst f     =  f &&& identity

> forkSecond      :: Arrow a => a b c -> a b (b,c)
> forkSecond f    =  identity &&& f

> argument        :: Arrow a => (b -> c) -> a c d -> a b d
> argument        =  (^>>)

> result          :: Arrow a => (c -> d) -> a b c -> a b d
> result          =  (^<<)

> argResult       :: Arrow a => (b -> c) -> (d -> e) -> a c d -> a b e
> argResult f g a =  f ^>> a >>^ g

> apply           :: Arrow a => a b (c -> d) -> a b c -> a b d
> apply f g       =  f &&& g >>> arr (\(f,c) -> f c)

> applyTo         :: Arrow a => a b c -> a b (c -> d) -> a b d
> applyTo         =  flip apply

