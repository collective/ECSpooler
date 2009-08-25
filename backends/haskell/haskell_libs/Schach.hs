module Schach where

data Col = A|B|C|D|E|F|G|H
   deriving (Eq, Ord, Enum, Show, Read)

data Row = One|Two|Three|Four|Five|Six|Seven|Eight
   deriving (Eq, Ord, Enum, Show, Read)

type Feld = (Col,Row)

data Steine = Koenig|Dame|Turm|Laeufer|Springer|Bauer
  deriving (Eq, Show, Read)
