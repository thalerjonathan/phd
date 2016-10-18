{-# LANGUAGE
             GADTs,
             KindSignatures,
             TypeOperators,
             EmptyDataDecls,
             TypeFamilies
    #-}

module Decoupled where


-- Decoupledness --------------------------------------------------------

data Dec :: *
data Cau :: *

------------------------------------------------------------------------

infixr 2 :|:
infixr 3 :&:

type family   d1  :|: d2  :: *
type instance Cau :|: d2  =  d2
type instance Dec :|: d2  =  Dec

type family   d1  :&: d2  :: *
type instance Cau :&: d2  =  Cau
type instance Dec :&: d2  =  d2

------------------------------------------------------------------------

data DRep :: * -> * where
  Dec :: DRep Dec
  Cau :: DRep Cau

-------------------------------------------------------------------------
