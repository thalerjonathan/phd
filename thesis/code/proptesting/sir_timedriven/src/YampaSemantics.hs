
switchTests :: Int -> [[(Double, Int)]]
switchTests n = embed (stepsSF sfs xs) ((), dts)
  where
    xs  = [0]
    sfs = [testSf]
    dts = replicate n (1.0, Nothing)

stepsSF :: [SF Int (Double, Int)] 
        -> [Int]
        -> SF () [(Double, Int)]
stepsSF sfs xs =
    dpSwitch 
      (\_ sfs' -> trace ("route " ++ show xs) zip xs sfs')
      sfs
      (switchingEvt >>> trace ("notYet " ++ show xs) notYet)
      stepsSF
  where
    switchingEvt :: SF ((), [(Double, Int)]) (Event [Int])
    switchingEvt = arr (\(_, xs') -> trace ("Event" ++ show xs) Event (map snd xs'))

testSf :: SF Int (Double, Int)
testSf = proc x -> do
  t <- time -< ()
  returnA -< trace ("received " ++ show x ++ " at t = " ++ show t) (t, x+1)

testSf0 :: SF Int (Double, Int)
testSf0 = switch
            testSf0Aux
            (const testSf1)
  where
    testSf0Aux :: SF Int ((Double, Int), Event ())
    testSf0Aux = proc _x -> do
      t <- time -< () 
      returnA -< ((t, 1), Event())

testSf1 :: SF Int (Double, Int)
testSf1 = switch
            testSf1Aux
            (const testSf2)
  where
    testSf1Aux :: SF Int ((Double, Int), Event ())
    testSf1Aux = proc _x -> do
      t <- time -< ()
      returnA -< ((t, 2), Event ())

testSf2 :: SF Int (Double, Int)
testSf2 = proc _x -> do
  t <- time -< ()
  returnA -< (t, 3)


dpSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
        -- the transition function at t=0, taking an argument a0 which is the 
        -- input to the routing funcion rf
        tf0 a0 =
            -- using the routing-function to pair up the input a0 with 
            -- the signal functions, resulting in (b, SF)
            let bsfs0 = rf a0 sfs0
            -- running the signal functions at t=0, simply passing their input
            -- and collecting to the transition function and collecting their
            -- outpus which are: (SF, [c])
                sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
            -- the outputs of the signal functions at t=0
                cs0   = fmap snd sfcs0
            in
                -- running the SF generating the event which takes the initial
                -- input at t=0 a0 and the outputs of the signal functions
                -- returns either NoEvent or Event with data passed to the
                -- continuation function k
                (case (sfTF sfe0) (a0, cs0) of
                     -- NoEvent, continue with new SFs 
                     (sfe, NoEvent)  -> dpSwitchAux (fmap fst sfcs0) sfe
                     -- Event, invoke continuation and pass new SFs and output cs0
                     -- of signal functions at t = 0
                     (_,   Event d0) -> fst (sfTF (k sfs0 d0) a0),cs0)

        -- the signal functions and the event generating signal function
        dpSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
        dpSwitchAux sfs sfe = SF' tf -- False
            where
                -- transition function for t > 0, takes dt and input at t > 0
                tf dt a =
                    -- routing function pairs up
                    let bsfs  = rf a sfs
                    -- now run signal functions with dt 
                        sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
                    -- get outputs of signal functions
                        cs    = fmap snd sfcs'
                    in
                        -- generate event with input a and output of signal functions 
                        -- at t > 0
                        (case (sfTF' sfe) dt (a, cs) of
                             (sfe', NoEvent) -> dpSwitchAux (fmap fst sfcs') sfe'
                             (_,    Event d) -> fst (sfTF (k (freezeCol sfs dt) d)),cs)