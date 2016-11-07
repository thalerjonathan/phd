{- $Id: Colors.hs,v 1.3 2004/11/18 13:02:26 henrik Exp $
******************************************************************************
*                              I N V A D E R S                               *
*                                                                            *
*       Module:		Colors                                       	     *
*       Purpose:	Colour definitions.				     *
*       Author:		Henrik Nilsson                                       *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module Colors (Color(..), RGB, colorTable) where

import Data.Array
import Graphics.HGL (RGB(..))


------------------------------------------------------------------------------
-- Color definitions
------------------------------------------------------------------------------

-- Pretty arbitrary selection of colours.
data Color =
-- Basic colours.
      Black
    | Blue
    | Green
    | Cyan
    | Red
    | Magenta
    | Yellow
    | White
-- Various greys.
    | DarkGrey
    | DimGrey
    | Grey
    | LightGrey
    | DarkSlateGrey
    | SlateGrey
    | LightSlateGrey
-- Various blues/cyan.
    | MidnightBlue
    | NavyBlue
    | CornflowerBlue
    | DarkSlateBlue
    | SlateBlue
    | LightSlateBlue
    | MediumBlue
    | RoyalBlue
    | DeepSkyBlue
    | SteelBlue
    | CadetBlue
-- Various greens/olive greens/khaki.
    | DarkGreen
    | DarkOliveGreen
    | SeaGreen
    | MediumSeaGreen
    | LawnGreen
    | LimeGreen
    | ForestGreen
    | OliveDrab
    | DarkKhaki
    | Khaki
-- Various oranges/browns.
    | Goldenrod
    | DarkGoldenrod
    | SaddleBrown
    | Orange
-- Various violets/purples.
    | Maroon
    | MediumVioletRed
    | VioletRed
    | Violet
    | Plum
    | Orchid
    | MediumOrchid
    | DarkOrchid
    | BlueViolet
    | Purple
    deriving (Eq, Ord, Bounded, Enum, Ix)

colorList = 
    [
        -- Basic colours.
        (Black,			RGB   0   0   0),
	(Blue,			RGB   0   0 255),
	(Green,			RGB   0 255   0),
	(Cyan,			RGB   0 255 255),
	(Red,			RGB 255   0   0),
	(Magenta,		RGB 255   0 255),
	(Yellow,		RGB 255 255   0),
	(White,			RGB 255 255 255),

	-- Various greys.
	(DarkGrey,		RGB  64  64  64),
	(DimGrey,		RGB 105 105 105),
	(Grey,			RGB 190 190 190),
	(LightGrey,		RGB 211 211 211),
	(DarkSlateGrey,		RGB  47  79  79),
	(SlateGrey,		RGB 112 128 144),
	(LightSlateGrey,	RGB 119 136 153),

	-- Various blues/cyan.
	(MidnightBlue,		RGB  25  25 112),
	(NavyBlue,		RGB   0   0 128),
	(CornflowerBlue,	RGB 100 149 237),
	(DarkSlateBlue,		RGB  72  61 139),
	(SlateBlue,		RGB 106  90 205),
	(LightSlateBlue,	RGB 132 112 255),
	(MediumBlue,		RGB   0   0 205),
	(RoyalBlue,		RGB  65 105 225),
	(DeepSkyBlue,		RGB   0 191 255),
	(SteelBlue,		RGB  70 130 180),
	(CadetBlue,		RGB  95 158 160),

	-- Various greens/olive greens/khaki.
	(DarkGreen,		RGB   0 100   0),
	(DarkOliveGreen,	RGB  85 107  47),
	(SeaGreen,		RGB  46 139  87),
	(MediumSeaGreen,	RGB  60 179 113),
	(LawnGreen,		RGB 124 252   0),
	(LimeGreen,		RGB  50 205  50),
	(ForestGreen,		RGB  34 139  34),
	(OliveDrab,		RGB 107 142  35),
	(DarkKhaki,		RGB 189 183 107),
	(Khaki,			RGB 240 230 140),

	-- Various oranges/browns.
	(Goldenrod,		RGB 218 165  32),
	(DarkGoldenrod,		RGB 184 134  11),
	(SaddleBrown,		RGB 139  69  19),
	(Orange,		RGB 255 165   0),

	-- Various violets/purples.
	(Maroon,		RGB 176  48  96),
	(MediumVioletRed,	RGB 199  21 133),
	(VioletRed,		RGB 208  32 144),
	(Violet,		RGB 238 130 238),
	(Plum,			RGB 221 160 221),
	(Orchid,		RGB 218 112 214),
	(MediumOrchid,		RGB 186  85 211),
	(DarkOrchid,		RGB 153  50 204),
	(BlueViolet,		RGB 138  43 226),
	(Purple,		RGB 160  32 240)
    ]


colorTable = array (minBound, maxBound) colorList
