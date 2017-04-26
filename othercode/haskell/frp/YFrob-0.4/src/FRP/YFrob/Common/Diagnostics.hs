{-
******************************************************************************
*                          Y F R O B / C O M M O N                           *
*                                                                            *
*       Module:         Diagnostics					     *
*       Purpose:        Standardized error-reporting for YFrob		     *
*	Authors:	Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.Common.Diagnostics where

usrErr :: String -> String -> String -> a
usrErr mn fn msg = error ("YFrob." ++ mn ++ "." ++ fn ++ ": " ++ msg)

intErr :: String -> String -> String -> a
intErr mn fn msg = error ("[internal error] YFrob." ++ mn ++ "." ++ fn ++ ": "
                          ++ msg)
