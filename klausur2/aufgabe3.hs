data Formel = Wahr
    | Falsch
    | Var String
    | Nicht Formel
    | Und Formel Formel
    | Oder Formel Formel
    deriving (Show)

type Belegung = [(String, Bool)]

ausdruck :: Formel
ausdruck = (Und (Oder (Oder Wahr Falsch) (Var "x")) (Nicht (Nicht (Nicht (Und Wahr (Var "y"))))))

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

auswerten :: Belegung -> Formel -> Maybe Bool
auswerten _ Wahr = Just True
auswerten _ Falsch = Just False
auswerten b (Var name) = lookup name b
auswerten b (Nicht formel) = not <$> auswerten b formel
auswerten b (Und lhs rhs) = (&&) <$> auswerten b lhs <*> auswerten b rhs
auswerten b (Oder lhs rhs) = (||) <$> auswerten b lhs <*> auswerten b rhs

variablen :: Formel -> [String]
variablen Wahr = []
variablen Falsch = []
variablen (Var name) = [name]
variablen (Nicht formel) = variablen formel
variablen (Und lhs rhs) = variablen lhs ++ variablen rhs
variablen (Oder lhs rhs) = variablen lhs ++ variablen rhs

vereinfache :: Formel -> Formel
vereinfache Wahr = Wahr
vereinfache Falsch = Falsch
vereinfache (Var name) = Var name
vereinfache (Nicht formel) = case vereinfache formel of
    (Nicht formel') -> formel'
    formel' -> Nicht formel'
vereinfache (Und lhs rhs) = case (vereinfache lhs, vereinfache rhs) of
    (Wahr, rhs) -> vereinfache rhs
    (lhs, Wahr) -> vereinfache lhs
    (Falsch, _) -> Falsch
    (_, Falsch) -> Falsch
    (lhs', rhs') -> Und lhs' rhs'
vereinfache (Oder lhs rhs) = case (vereinfache lhs, vereinfache rhs) of
    (Falsch, rhs) -> vereinfache rhs
    (lhs, Falsch) -> vereinfache lhs
    (Wahr, _) -> Wahr
    (_, Wahr) -> Wahr
    (lhs', rhs') -> Oder lhs' rhs'
