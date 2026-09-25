class Form a where
    flaeche :: a -> Double
    umfang :: a -> Double

data Kreis = Kreis Double
data Rechteck = Rechteck Double Double

instance Form Kreis where
    flaeche (Kreis radius) = radius * radius * pi
    umfang (Kreis radius) = 2 * radius * pi

instance Form Rechteck where
    flaeche (Rechteck w h) = w * h
    umfang (Rechteck w h) = 2 * (w + h)

kompaktheit :: Form a => a -> Double
kompaktheit form = 4 * pi * flaeche' / umfang' / umfang'
    where flaeche' = flaeche form; umfang' = umfang form

data Note = SehrGut | Gut | Befriedigend | Ausreichend | Mangelhaft
    deriving (Eq, Ord, Show)
