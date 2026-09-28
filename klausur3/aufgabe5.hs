data ZaehlerAPI r = Wert (Int -> r) | Name (String -> r)
type Zaehler r = ZaehlerAPI r -> r

newZaehler :: String -> Int -> ZaehlerAPI r -> r -- Zaehler r
newZaehler name startwert api = case api of
    Wert use -> use startwert
    Name use -> use name

main = do
    let zaehler = newZaehler "name" 67
    print $ zaehler $ Wert (+2)
