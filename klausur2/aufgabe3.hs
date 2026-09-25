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
vereinfache (Nicht (Nicht formel)) = vereinfache formel
vereinfache (Und Wahr rhs) = vereinfache rhs
vereinfache (Und lhs Wahr) = vereinfache lhs
vereinfache (Und Falsch _) = Falsch
vereinfache (Und _ Falsch) = Falsch
vereinfache (Oder Falsch rhs) = vereinfache rhs
vereinfache (Oder lhs Falsch) = vereinfache lhs
vereinfache (Oder Wahr _) = Wahr
vereinfache (Oder _ Wahr) = Wahr
vereinfache Wahr = Wahr
vereinfache Falsch = Falsch
vereinfache (Var name) = Var name
vereinfache (Nicht formel) = Nicht $ vereinfache formel
vereinfache (Und lhs rhs) = Und (vereinfache lhs) (vereinfache rhs)
vereinfache (Oder lhs rhs) = Oder (vereinfache lhs) (vereinfache rhs)
