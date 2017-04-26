{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		IdentityList					     *
*       Purpose:	Association list with automatic key assignment and   *
*			identity-preserving map and filter operations.	     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

-- ToDo:
-- * Change names of ilKeys, ilElems, ilAssocs to keysIL, elemsIL, assocsIL.
--   (Keep name of fields in IL type, thus assocsIL = ilAssocs.)

module FRP.YFrob.RobotSim.IdentityList (
    ILKey,	  -- Identity-list key type
    IL,		  -- Identity-list, abstract. Instance of functor.
    emptyIL,	  -- :: IL a
    insertIL_,	  -- :: a -> IL a -> IL a
    insertIL,	  -- :: a -> IL a -> (ILKey, IL a)
    listToIL,	  -- :: [a] -> IL a
    ilKeys,	  -- :: IL a -> [ILKey]
    ilElems,	  -- :: IL a -> [a]
    ilAssocs,	  -- :: IL a -> [(ILKey, a)]
    deleteIL,	  -- :: ILKey -> IL a -> IL a
    mapIL,	  -- :: ((ILKey, a) -> b) -> IL a -> IL b
    filterIL,	  -- :: ((ILKey, a) -> Bool) -> IL a -> IL a
    mapFilterIL,  -- :: ((ILKey, a) -> Maybe b) -> IL a -> IL b
    lookupIL,	  -- :: ILKey -> IL a -> Maybe a
    findIL,	  -- :: ((ILKey, a) -> Bool) -> IL a -> Maybe a
    mapFindIL,	  -- :: ((ILKey, a) -> Maybe b) -> IL a -> Maybe b
    findAllIL,	  -- :: ((ILKey, a) -> Bool) -> IL a -> [a]
    mapFindAllIL  -- :: ((ILKey, a) -> Maybe b) -> IL a -> [b]
) where

------------------------------------------------------------------------------
-- Data type definitions
------------------------------------------------------------------------------

type ILKey = Int

-- Invariants:
-- * List sorted in descending key order.
-- * Keys never reused.
data IL a = IL { ilNextKey :: ILKey, ilAssocs :: [(ILKey, a)] }


------------------------------------------------------------------------------
-- Class instances
------------------------------------------------------------------------------

instance Functor IL where
    fmap f (IL {ilNextKey = nk, ilAssocs = kas}) =
        IL {ilNextKey = nk, ilAssocs = [ (i, f a) | (i, a) <- kas ]}


------------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------------

emptyIL :: IL a
emptyIL = IL {ilNextKey = 0, ilAssocs = []}


insertIL_ :: a -> IL a -> IL a
insertIL_ a il = snd (insertIL a il)


insertIL :: a -> IL a -> (ILKey, IL a)
insertIL a (IL {ilNextKey = k, ilAssocs = kas}) = (k, il') where
    il' = IL {ilNextKey = k + 1, ilAssocs = (k, a) : kas}


listToIL :: [a] -> IL a
listToIL as = IL {ilNextKey = length as,
		  ilAssocs = reverse (zip [0..] as)} -- Maintain invariant!


------------------------------------------------------------------------------
-- Additional selectors
------------------------------------------------------------------------------

ilKeys :: IL a -> [ILKey]
ilKeys = map fst . ilAssocs


ilElems :: IL a -> [a]
ilElems = map snd . ilAssocs


------------------------------------------------------------------------------
-- Mutators
------------------------------------------------------------------------------

deleteIL :: ILKey -> IL a -> IL a
deleteIL k (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = deleteHlp kas}
    where
	deleteHlp []                                   = []
        deleteHlp kakas@(ka@(k', _) : kas) | k > k'    = kakas
					   | k == k'   = kas
                                           | otherwise = ka : deleteHlp kas


------------------------------------------------------------------------------
-- Filter and map operations
------------------------------------------------------------------------------

-- These are "identity-preserving", i.e. the key associated with an element
-- in the result is the same as the key of the element from which the
-- result element was derived.

mapIL :: ((ILKey, a) -> b) -> IL a -> IL b
mapIL f (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = [(k, f ka) | ka@(k,_) <- kas]}


filterIL :: ((ILKey, a) -> Bool) -> IL a -> IL a
filterIL p (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = filter p kas}


mapFilterIL :: ((ILKey, a) -> Maybe b) -> IL a -> IL b
mapFilterIL p (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {
        ilNextKey = nk,
        ilAssocs = [(k, b) | ka@(k, _) <- kas, Just b <- [p ka]]
    }


------------------------------------------------------------------------------
-- Lookup operations
------------------------------------------------------------------------------

lookupIL :: ILKey -> IL a -> Maybe a
lookupIL k il = lookup k (ilAssocs il)


findIL :: ((ILKey, a) -> Bool) -> IL a -> Maybe a
findIL p (IL {ilAssocs = kas}) = findHlp kas
    where
	findHlp []                = Nothing
        findHlp (ka@(_, a) : kas) = if p ka then Just a else findHlp kas


mapFindIL :: ((ILKey, a) -> Maybe b) -> IL a -> Maybe b
mapFindIL p (IL {ilAssocs = kas}) = mapFindHlp kas
    where
	mapFindHlp []         = Nothing
        mapFindHlp (ka : kas) = case p ka of
				    Nothing     -> mapFindHlp kas
				    jb@(Just _) -> jb


findAllIL :: ((ILKey, a) -> Bool) -> IL a -> [a]
findAllIL p (IL {ilAssocs = kas}) = [ a | ka@(_, a) <- kas, p ka ]


mapFindAllIL:: ((ILKey, a) -> Maybe b) -> IL a -> [b]
mapFindAllIL p (IL {ilAssocs = kas}) = [ b | ka <- kas, Just b <- [p ka] ]
