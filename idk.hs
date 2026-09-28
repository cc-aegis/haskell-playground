auswerten b (Oder lhs rhs) =
    auswerten b lhs >>= \ lhs' ->
    auswerten b rhs >>= \ rhs' ->
    Just (lhs' || rhs')

auswerten b (Oder lhs rhs) = (||) <$> auswerten b lhs <*> auswerten b rhs
