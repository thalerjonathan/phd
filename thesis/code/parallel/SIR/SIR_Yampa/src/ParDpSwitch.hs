{-# LANGUAGE RankNTypes #-}

module ParDpSwitch where

-- import Control.Parallel
import Control.Parallel.Strategies

import FRP.Yampa.Event
import FRP.Yampa.InternalCore

dpSwitchParEval :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf))) -- ^ Routing function. Its purpose is
                                               --   to pair up each running signal function in the collection
                                               --   maintained by 'dpSwitch' with the input it is going to see
                                               --   at each point in time. All the routing function can do is specify
                                               --   how the input is distributed.
    -> col (SF b c)                            -- ^ Initial collection of signal functions.
    -> SF (a, col c) (Event d)                 -- ^ Signal function that observes the external
                                               --   input signal and the output signals from the collection in order
                                               --   to produce a switching event.
    -> (col (SF b c) -> d -> SF a (col c))     -- ^ The fourth argument is a function that is invoked when the
                                               --   switching event occurs, yielding a new signal function to switch
                                               --   into based on the collection of signal functions previously
                                               --   running and the value carried by the switching event. This
                                               --   allows the collection to be updated and then switched back
                                               --   in, typically by employing 'dpSwitch' again.
    -> SF a (col c)
dpSwitchParEval rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let bsfs0 = rf a0 sfs0
                sfcs0 = fmap (\(b0, sf0) -> using ((sfTF sf0) b0) rpar ) bsfs0
                cs0   = fmap snd sfcs0
            in
                (case (sfTF sfe0) (a0, cs0) of
                     (sfe, NoEvent)  -> dpSwitchAux (fmap fst sfcs0) sfe
                     (_,   Event d0) -> fst (sfTF (k sfs0 d0) a0),
                 cs0)

        dpSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
        dpSwitchAux sfs sfe = SF' tf -- False
            where
                tf dt a =
                    let bsfs  = rf a sfs
                        sfcs' = fmap (\(b, sf) -> using ((sfTF' sf) dt b) rpar) bsfs
                        cs    = fmap snd sfcs'
                    in
                        (case (sfTF' sfe) dt (a, cs) of
                             (sfe', NoEvent) -> dpSwitchAux (fmap fst sfcs')
                                                            sfe'
                             (_,    Event d) -> fst (sfTF (k (freezeCol sfs dt)
                                                             d)
                                                          a),
                         cs)

-- Internal definition. Also used in parallel switchers.
parAux :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF' b c)
    -> SF' a (col c)
parAux rf sfs = SF' tf -- True
    where
        tf dt a =
            let bsfs  = rf a sfs
                sfcs' = fmap (\(b, sf) -> using ((sfTF' sf) dt b) rpar) bsfs
                sfs'  = fmap fst sfcs'
                cs    = fmap snd sfcs'
            in
                (parAux rf sfs', cs)

-- Freezes a "running" signal function, i.e., turns it into a continuation in
-- the form of a plain signal function.
freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}

freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (`freeze` dt) sfs
