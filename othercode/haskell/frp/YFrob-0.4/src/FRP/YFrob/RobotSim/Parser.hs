{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:         Parser						     *
*       Purpose:        Parsing (mainly lexical analysis) of window event    *
*			stream.					             *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

-- To do:
-- * Quick 'n dirty adaptation from old simulator. Could probably be done
--   better in the new Yampa framework.
-- * It is questionable to what extent the scanner should know about
--   simulator modes! Maybe one should just have one single set of commands. 
-- * Use e.g ':' as a general prefix for long commands?
-- * Re-evaluate the stateful (mode dependent) scanning strategy.
--   E.g. suppose we'd like a single common character to abandon the
--   entire command. This is simple if we can goto a single, globally
--   known state. With the current approach, we'd have to add failure
--   continuations all over the place.

{-

The lexical scanner for character input in effect implements a simple,
interactive, command parser. The following general rules apply:

* A command consists of a sequence of one or more fields.
* The first field is the command keword.
* The number of fields and the length of each fields are known, but not
  necessarily fixed; i.e., they can be computed from the command input
  seen thus far.
* The scanner will start reading the next field as soon as a field is
  complete and the next command as soon as a command is complete
  (i.e. the last field is complete).
* The return key can be used to terminate a field early. If the input
  in the field thus far is valid (e.g. a valid command or a valid integer),
  then the field is considered to be complete and the scanner starts
  reading the next field (or next command, if this was the last field).
  Otherwise nothing happens (the return character is discarded).
* Any invalid character (backspace and delete should always be invalid)
  will cause the reading of a field to be restarted.
* The character '.' is used to complete command keywords. (Not much use
  currently since all commands have very short names.)

The simulator can be in one the the three modes: Edit, Run and Frozon.
Different commands are available in these three modes.

Edit Mode:

The simulator is in this mode initially.  Commands in this mode are:

    cr <robot>
        creates a robot, where <robot> can be:
            a       -- a robot of type Simbot A (round)
            b       -- a robot of type Simbot B (triangular)

    co <obstacle>
        creates an obstacle, where <obstacle> can be:
            block   -- a quare block
	    nswall  -- a north-south wall
	    ewwall  -- an east-west wall

    cb
	creates a ball

    select <objects>
        adds zero or more objects to the current selection, where
        <objects> can be:
	    block    -- all block obstacles
	    nswall   -- all north-south walls
	    ewwall   -- all east-west walls
	    wall     -- all walls
	    obst     -- all obstacles
	    simbota  -- all robots of type Simbot A
	    simbotb  -- all robots of type Simbot B
	    robot    -- all robots
            all      -- all robots and obstacles
        Besides, you can also left click to select a single object.

    p   selects the previous object

    n   selects the next object

    u   unselects all

    tl  turns all robots selected left

    tr  turns all robots selected right

    tn  turns all robots selected north

    te  turns all robots selected east

    ts  turns all robots selected south

    tw  turns all robots selected west

    tt <angle-in-degrees>
        turns all robots selected to a specific angle (in degrees), where
        0 is heading north, 90 is east, and so on

    delete
        deletes all objects selected

    go  lets all robots run

    quit
        quits the simulator (unimplemented yet, you'll have to close the
        window instead)

Run Mode:

The simulator enters the run mode once the "go" command is issued.
There are only two commands in this mode:

    f   freezes the robots and enters the frozen mode;  this is not fully
        implemented yet -- the mode is switched but the robots are not
        stopping

    edit
        freezes the robots and returns to the edit mode

Frozen Mode:

This mode is entered by the "f" command in the run mode.

    r   resumes the robots and goes back to the frozen mode

    edit
        swtiches to the edit mode
-}


module FRP.YFrob.RobotSim.Parser (
    SimInput,		-- Abstract
    parseWinInput,	-- :: SF WinInput SimInput
    command,		-- :: SF SimInput (Event Command)
    cmdString,		-- :: SF SimInput (Event String)
    ptrPos,		-- :: SF SimInput Position2
    lbp,		-- :: SF SimInput (Event ())
    lbpPos,		-- :: SF SimInput (Event Position2)
    lbDown,		-- :: SF SimInput Bool
    rbp,		-- :: SF SimInput (Event ())
    rbpPos,		-- :: SF SimInput (Event Position2)
    rbDown,		-- :: SF SimInput Bool
    dragStart,		-- :: SF SimInput (Event ())
    dragStop,		-- :: SF SimInput (Event Distance2)
    dragStartPos,	-- :: SF SimInput Position2
    dragVec,		-- :: SF SimInput Distance2
    dragging		-- :: SF SimInput Bool
) where

import Data.Maybe (isJust)
import Data.Char (ord, isSpace, isDigit)

import qualified Graphics.HGL as HGL (Event(..))

import FRP.Yampa
import FRP.Yampa.Geometry

import FRP.YFrob.Common.PhysicalDimensions

import FRP.YFrob.RobotSim.WorldGeometry (gPointToPosition2)
import FRP.YFrob.RobotSim.Command
import FRP.YFrob.RobotSim.Object (ObjClass(..))
import FRP.YFrob.RobotSim.Animate (WinInput)

------------------------------------------------------------------------------
-- Exported entities
------------------------------------------------------------------------------

data SimInput = SimInput {
    siCmdStr :: String,
    siCmd    :: Event Command,
    siPDS    :: PDState
}


parseWinInput :: SF WinInput SimInput
parseWinInput = wiToCmd &&& wiToPDS
                >>^ \((cmdStr, cmd), pds) ->
		        SimInput {siCmdStr = cmdStr, siCmd = cmd, siPDS = pds}


-- All event sources below are defined such that they will NOT occur at local
-- time 0 (immediately after a switch). Sometimes explicitly using a "notYet".
-- Sometimes using through careful use of "edge" and relatives. Is this the
-- right approach?

-- A valid command has been read.
command :: SF SimInput (Event Command)
command = siCmd ^>> notYet


-- Continuous parser feed back.
cmdString :: SF SimInput String
cmdString = arr siCmdStr


{-
-- Probably not needed. Old FRP code!
-- New command scanner state, represented as the current command prefix.
-- Empty string once a valid command has been scanned!
cmdStateChange :: SF SimInput (Event String)
siCmdStateChange :: Event SI String
siCmdStateChange = whileByE (fmap curPfx) (fmap siCmd inputB)
    where
        curPfx (pfx, mcmd) = maybe pfx (const "") mcmd
-}


ptrPos :: SF SimInput Position2
ptrPos = arr (pdsPos . siPDS)


lbp :: SF SimInput (Event ())
lbp = lbpPos >>^ (`tag` ())


lbpPos :: SF SimInput (Event Position2)
lbpPos = siPDS # pdsLeft ^>> edgeJust


lbDown :: SF SimInput Bool
lbDown = arr (siPDS # pdsLeft # isJust)


rbp :: SF SimInput (Event ())
rbp = rbpPos >>^ (`tag` ())


rbpPos :: SF SimInput (Event Position2)
rbpPos = siPDS # pdsRight ^>> edgeJust


rbDown :: SF SimInput Bool
rbDown = arr (siPDS # pdsRight # isJust)


dragStart :: SF SimInput (Event ())
dragStart = siPDS # pdsDrag ^>> edgeBy detectStart (Just undefined)
    where
        detectStart Nothing  (Just _) = Just ()
        detectStart _        _        = Nothing


dragStop :: SF SimInput (Event Distance2)
dragStop = (siPDS # pdsDrag ^>> edgeBy detectStop Nothing) &&& dragVec
           >>^ \(e, dv) -> e `tag` dv
    where
        detectStop (Just _) Nothing = Just ()
        detectStop _        _       = Nothing


-- (Last) drag start position.
dragStartPos :: SF SimInput Position2
dragStartPos = arr (siPDS # pdsDragStartPos)


-- (Last) drag vector.
dragVec :: SF SimInput Distance2
dragVec = arr (siPDS # pdsDragVec)


dragging :: SF SimInput Bool
dragging = arr (siPDS # pdsDrag # isJust)


------------------------------------------------------------------------------
-- Lexical analysis of character input 
------------------------------------------------------------------------------

wiToCmd :: SF WinInput (String, Event Command)
wiToCmd = arr (mapFilterE selChar)
          >>> (accumBy scanChar (undefined,scanCmdsEdit)
               >>^ fmap fst >>^ splitE)
          >>> hold "" *** arr (mapFilterE id)
    where
        scanChar (_, S cont) c = cont c

        selChar (HGL.Char {HGL.char=c}) = Just c
	selChar _	                = Nothing


-- This ought to be redone. Kont should probably be called Tranition or
-- somethinig.

-- We define a continuation to be the command recognized thus far (a String
-- and maybe a complete Command), and a scanner to be applied to the rest
-- of the input. (I.e., there's output at every step.)

type Kont = ((String, Maybe Command), Scanner)
type Cont a = a -> Kont

-- Since a scanner is applied to one character at a time (typically, on
-- Char events), we recursively define a scanner to be a character
-- continuation.

newtype Scanner = S (Cont Char)

-- *** Note! It is questionable to what extent the scanner should know about
-- simulator modes! Maybe one should just have one single set of commands. 

-- Can only read lowercase and some symbols currently. In particular, cannot
-- read '!', so the commands "q!", "r!", and "e!" are out.


-- Scan commands in Edit mode.

scanCmdsEdit :: Scanner
scanCmdsEdit = scanCmd editCmds
    where
        editCmds =
	    [ ("quit",   emitCmd scanCmdsEdit CmdQuit), -- Discard inp.?
	      ("run",    emitCmd scanCmdsRun  CmdRun), 
	      ("co",     readObstClass),
	      ("cr",     readRobotClass),
	      ("cb",     emitCmd scanCmdsEdit CmdCreateBall),
	      ("delete", emitCmd scanCmdsEdit CmdDelete),
	      ("n",      emitCmd scanCmdsEdit CmdSelectNext),
	      ("p",      emitCmd scanCmdsEdit CmdSelectPrev),
	      ("select", readObjClass),
	      ("u",      emitCmd scanCmdsEdit CmdUnselectAll),
	      ("tl",     emitCmd scanCmdsEdit CmdTurnLeft),
	      ("tr",     emitCmd scanCmdsEdit CmdTurnRight),
	      ("tn",     emitCmd scanCmdsEdit
				 (CmdTurnTo (bearingToHeading 0))),
	      ("te",     emitCmd scanCmdsEdit
				 (CmdTurnTo (bearingToHeading 90))),
	      ("ts",     emitCmd scanCmdsEdit
				 (CmdTurnTo (bearingToHeading 180))),
	      ("tw",     emitCmd scanCmdsEdit
				 (CmdTurnTo (bearingToHeading 270))),
	      ("tt",     readAngle),
	      ("save",   readFilePathForSave),
	      ("open",   readFilePathForLoad)
	    ]

        readObstClass pfx = emitPfx (scanSubCmd (pfx ++ " ") coSubCmds) pfx

        coSubCmds =
	    [ ("block",  emitCmd scanCmdsEdit (CmdCreateObst ClsBlock)),
	      ("nswall", emitCmd scanCmdsEdit (CmdCreateObst ClsNSWall)),
	      ("ewwall", emitCmd scanCmdsEdit (CmdCreateObst ClsEWWall))
	    ]

	-- We will have to read further parameters here.
        readRobotClass pfx = emitPfx (scanSubCmd (pfx ++ " ") crSubCmds) pfx

        crSubCmds = [("a",  emitCmd scanCmdsEdit (CmdCreateRobot ClsSimbotA)),
		     ("b",  emitCmd scanCmdsEdit (CmdCreateRobot ClsSimbotB))]

        readObjClass pfx = emitPfx (scanSubCmd (pfx ++ " ") selectSubCmds) pfx

        selectSubCmds =
	    [ ("all",     emitCmd scanCmdsEdit (CmdSelect ClsObj)),
	      ("obst",    emitCmd scanCmdsEdit (CmdSelect ClsInanimate)),
	      ("block",   emitCmd scanCmdsEdit (CmdSelect ClsBlock)),
	      ("wall",    emitCmd scanCmdsEdit (CmdSelect ClsWall)),
	      ("nswall",  emitCmd scanCmdsEdit (CmdSelect ClsNSWall)),
	      ("ewwall",  emitCmd scanCmdsEdit (CmdSelect ClsEWWall)),
	      ("robot",   emitCmd scanCmdsEdit (CmdSelect ClsRobot)),
	      ("simbota", emitCmd scanCmdsEdit (CmdSelect ClsSimbotA)),
	      ("simbotb", emitCmd scanCmdsEdit (CmdSelect ClsSimbotB))
	    ]

        readAngle pfx =
	    emitPfx (scanIntegerArg pfx
				    3
				    (\(cmdStr, ang) ->
				         emitCmd scanCmdsEdit
					         (CmdTurnTo
						     (bearingToHeading
							  (fromInteger ang)))
					         cmdStr))
		    pfx

        readFilePathForSave pfx =
	    emitPfx (scanStringArg pfx
		                   (\(cmdStr, path) ->
		                        emitCmd scanCmdsEdit
		                                (CmdSave path)
		                                cmdStr))
		    pfx
						 
        readFilePathForLoad pfx =
	    emitPfx (scanStringArg pfx
		                   (\(cmdStr, path) ->
		                        emitCmd scanCmdsEdit
		                                (CmdLoad path)
		                                cmdStr))
		    pfx
						 

-- Scan commands in Run mode.

scanCmdsRun :: Scanner
scanCmdsRun = scanCmd runCmds
    where
        runCmds =
	    [ ("f",  emitCmd scanCmdsFrozen CmdFreeze),
	      ("edit", emitCmd scanCmdsEdit   CmdEdit)
	    ]


-- Scan commands in Frozen mode.

scanCmdsFrozen :: Scanner
scanCmdsFrozen = scanCmd frozenCmds
    where
        frozenCmds =
	    [ ("r",  emitCmd scanCmdsRun  CmdResume),
	      ("edit", emitCmd scanCmdsEdit CmdEdit)
	    ]


-- Scan one command.
-- Looks for a valid command. Outputs prefix as long as the current
-- prefix is valid. Starts over on first invalid character. Invokes success
-- continuation on success.
-- cmds ....... List of pairs of valid command and corresponding success
--		continuation. 

scanCmd :: [(String, Cont String)] -> Scanner
scanCmd cmds = scanSubCmd "" cmds


-- Scan one subcommand/keyword argument.
-- Looks for a valid command. Outputs prefix as long as the current
-- prefix is valid. Starts over on first invalid character. Invokes success
-- continuation on success.
-- pfx0 ....... Initial prefix.
-- cmds ....... List of pairs of valid command and corresponding success
--		continuation. 

scanSubCmd :: String -> [(String, Cont String)] -> Scanner
scanSubCmd pfx0 cmds = S (scHlp pfx0 cmds)
    where
        -- pfx ........	Command prefix.
        -- sfxconts ...	Command suffixes paired with success continuations.
        -- c .......... Input character.
        scHlp pfx sfxconts c =
	    case c of
	        '\r' ->
		    case [ cont | ("", cont) <- sfxconts ] of
		       []         -> emitPfx (S (scHlp pfx sfxconts)) pfx
		       (cont : _) -> cont pfx
		'.'  ->
		    case sfxconts of
		        []            -> emitPfx (S (scHlp pfx0 cmds)) pfx0
			[(sfx, cont)] -> cont (pfx ++ sfx)
			_             ->
			    let
			        (sfxs, conts) = unzip sfxconts
				cpfx          = foldr1 lcp sfxs
				sfxs'         = map (drop (length cpfx)) sfxs
				pfx'	      = pfx ++ cpfx
				sfxconts'     = zip sfxs' conts
			    in
			        emitPfx (S (scHlp pfx' sfxconts')) pfx'
		_    ->
		    let
		        pfx' = pfx ++ [c]
			sfxconts' = [ (tail sfx, cont)
			            | (sfx, cont) <- sfxconts,
				      not (null sfx) && head sfx == c
				    ]
		    in
		        case sfxconts' of
			    []           -> emitPfx (S (scHlp pfx0 cmds))
						    pfx0
						    -- ("Invalid: " ++ [c])
			    [("", cont)] -> cont pfx'
			    _            -> emitPfx (S (scHlp pfx' sfxconts'))
						    pfx'


-- Scan fixed-length integer argument.
-- pfx0 .......	Initial prefix (command scanned thus far).
-- n0 .........	Maximal number of digits.
-- cont .......	Continuation: will be passed the new prefix and the
--		integer value of the scanned argument.

scanIntegerArg :: String -> Int -> Cont (String,Integer) -> Scanner
scanIntegerArg pfx0 n0 cont | n0 > 0 = S (siaHlp (pfx0 ++ " ") n0 0)
    where
        siaHlp pfx n a c =
	    if c == '\r' then
	        cont (pfx, a)
	    else if isDigit c then
	        let a'   = a * 10 + fromIntegral (ord c - ord '0')
		    pfx' = pfx ++ [c]
		in
		    if n > 1 then
		        emitPfx (S (siaHlp pfx' (n - 1) a')) pfx'
		    else
			cont (pfx', a')
	    else
	        emitPfx (S (siaHlp (pfx0 ++ " ") n0 0)) pfx0


-- Scan variable-length string argument.
-- pfx0 .......	Initial prefix (command scanned thus far).
-- cont .......	Continuation: will be passed the new prefix and the
--		string value of the scanned argument.

scanStringArg :: String -> Cont (String,String) -> Scanner
scanStringArg pfx0 cont = S (ssaHlp (pfx0 ++ " ") "")
    where
        ssaHlp pfx a c =
	    if c == '\r' then
	        cont (pfx, a)
	    else
	        let a'   = dropWhile isSpace $ a ++ [c]
		    pfx' = pfx ++ [c]
		in
		    emitPfx (S (ssaHlp pfx' a')) pfx'


-- Emit command (and command string), then continue scanning.
emitCmd :: Scanner -> Command -> String -> Kont
emitCmd scanner cmd cmdStr = ((cmdStr, Just cmd), scanner)


-- Emit current prefix, then scan next character.
emitPfx :: Scanner -> String -> Kont
emitPfx scanner pfx = ((pfx, Nothing), scanner)


------------------------------------------------------------------------------
-- Pointing device processing
------------------------------------------------------------------------------

-- State of the pointing device.
-- The points for pdsLeft, pdsRight, and pdsDrag reflect where the button
-- was initially pressed.


data PDState = PDState {
    pdsPos          :: Position2,		-- Current position.
    pdsDragStartPos :: Position2,		-- (Last) drag start position.
    pdsDragVec      :: Distance2,		-- (Latest) drag vector.
    pdsLeft         :: Maybe Position2,		-- Left button currently down.
    pdsRight        :: Maybe Position2,		-- Right button currently down.
    pdsDrag         :: Maybe Position2		-- Currently dragging.
--    pdsPrevLeft :: Maybe Position2,		-- Previous left button state.
--    pdsPrevRight:: Maybe Position2,		-- Previous right button state.
--    pdsPrevDrag :: Maybe Position2 		-- Previous drag state.
}


-- Initial state.
initPDS :: PDState
initPDS = PDState {
	      pdsPos          = origin,
	      pdsDragStartPos = origin,
	      pdsDragVec      = zeroVector,
	      pdsLeft         = Nothing,
	      pdsRight        = Nothing,
	      pdsDrag         = Nothing
--	      pdsPrevLeft     = Nothing,
--	      pdsPrevRight    = Nothing,
--	      pdsPrevDrag     = Nothing
	  }


wiToPDS :: SF WinInput PDState
wiToPDS = accumHoldBy nextPDS initPDS


{-
-- Left-over from the "prev" mechanism that hopefully will not be needed.
updPrev pds (PDState {pdsLeft = pl, pdsRight = pr, pdsDrag = pd}) =
    pds {pdsPrevLeft = pl, pdsPrevRight = pr, pdsPrevDrag = pd}
-}


-- Compute next pointing device state.
nextPDS :: PDState -> HGL.Event -> PDState
nextPDS pds (HGL.Key {}) = pds			-- Currently we ignore keys.
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = True, HGL.isDown = True}) =
    -- Left button pressed.
    pds {pdsPos = p', pdsDragVec = dv, pdsLeft = Just p'}
    where
        p' = gPointToPosition2 p
	dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) (pdsDrag pds)
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = True, HGL.isDown = False}) =
    -- Left button released.
    pds {pdsPos = p', pdsDragVec = dv, pdsLeft = Nothing, pdsDrag = md}
    where
        p' = gPointToPosition2 p
        md = maybe Nothing (const (pdsDrag pds)) (pdsRight pds)
	dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) md
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = False, HGL.isDown = True}) =
    -- Right button pressed.
    pds {pdsPos = p', pdsDragVec = dv, pdsRight = Just p'}
    where
        p' = gPointToPosition2 p
	dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) (pdsDrag pds)
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = False, HGL.isDown = False}) =
    -- Right button released.
    pds {pdsPos = p', pdsDragVec = dv, pdsRight = Nothing, pdsDrag = md}
    where
        p' = gPointToPosition2 p
        md = maybe Nothing (const (pdsDrag pds)) (pdsLeft pds)
	dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) md
nextPDS pds (HGL.MouseMove {HGL.pt = p}) =
    -- Mouse move.
    pds {pdsPos = p', pdsDragStartPos = dsp, pdsDragVec = dv, pdsDrag = md}
    where
        p' = gPointToPosition2 p
        md = case pdsLeft pds of
	         mlp@(Just _) -> mlp
		 Nothing      -> pdsRight pds
        dsp = maybe (pdsDragStartPos pds) id md
	dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) md
nextPDS pds _ = pds				-- Ignore unknown events.


------------------------------------------------------------------------------
-- General utilities
------------------------------------------------------------------------------

-- Longest common prefix.
lcp :: Eq a => [a] -> [a] -> [a]
lcp []     _                  = []
lcp _      []                 = []
lcp (x:xs) (y:ys) | x == y    = x : lcp xs ys
                  | otherwise = []
