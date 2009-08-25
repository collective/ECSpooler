module Kreise where

import SetKreise

type Punkt = (Double,Double)
type Kreis = (Punkt,Double)

-- Bestimmt den euklidischen Abstand zweier Punkte
abstandEuklid :: Punkt -> Punkt -> Double
abstandEuklid (x1,y1) (x2,y2) = sqrt (((x1-x2)^2)+((y1-y2)^2))

-- Prüft, ob sich ein Punkt im Kreis oder auf der Kreislinie befindet
punktImKreis :: Punkt -> Kreis -> Bool
punktImKreis p (m,r) = abstandEuklid p m <= r

-- Restmenge jener a's, die mit allen b's eine Beziehung r eingehen
restmenge :: ((a,b) -> Bool) -> Set a -> Set b -> Set a
restmenge r as bs = filterSet (\a -> allSet ((curry r) a) bs) as

-- Restmenge der Punkte, die in keinem der gegebenen Kreise liegen
freiePunkte :: Set Punkt -> Set Kreis -> Set Punkt
freiePunkte = restmenge (not . (uncurry punktImKreis))

-- Restmenge der Kreise, die keinen der gegebenen Punkte enthalten
leereKreise :: Set Kreis -> Set Punkt -> Set Kreis
leereKreise = restmenge (not . (uncurry (flip punktImKreis)))
