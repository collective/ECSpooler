module Datum where

data Jahreszeit = Fruehling | Sommer | Herbst | Winter
   deriving (Eq, Ord, Enum, Show, Read)

data Monat = Januar | Februar | Maerz | April | Mai | Juni | Juli |
             August | September | Oktober | November | Dezember
   deriving (Eq, Ord, Enum, Show, Read)

type Jahr = Int
type Tag = Int