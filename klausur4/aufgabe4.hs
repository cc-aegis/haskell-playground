data Ware = Ware String Double Int
    deriving Show

gesamtwert :: [Ware] -> Double
gesamtwert [] = 0
gesamtwert (Ware _ wert menge : ws) = wert * fromIntegral menge + gesamtwert ws

(|>) = flip ($)

gesamtwert' :: [Ware] -> Double
gesamtwert' waren = waren
    |> map (\(Ware _ wert menge) -> wert * fromIntegral menge)
    |> sum

gesamtwert'' :: [Ware] -> Double
gesamtwert'' = sum . map (\ (Ware _ wert menge) -> wert * fromIntegral menge)

-- gesamtwert''' :: [Ware] -> Double
-- gesamtwert''' = (|> map (\(Ware _ wert menge) -> wert * fromIntegral menge) |> sum)

teuer :: Double -> [Ware] -> [String]
teuer grenze waren = [name | (Ware name wert _) <- waren, wert > grenze]

teuer10 :: [Ware] -> [String]
teuer10 = teuer 10

nachfuellen :: Int -> [Ware] -> [Ware]
nachfuellen _ [] = []
nachfuellen mass (Ware name wert menge : rest)
    | menge < 3 = Ware name wert (menge + mass) : nachfuellen mass rest
    | otherwise = Ware name wert menge : nachfuellen mass rest

einfuegen :: Ware -> [Ware] -> [Ware]
einfuegen w [] = [w]
einfuegen ware@(Ware _ preis _) (ware'@(Ware _ preis' _) : rest)
    | preis < preis' = ware : ware' : rest
    | otherwise = ware' : einfuegen ware rest

sortiere :: [Ware] -> [Ware]
sortiere [] = []
sortiere (w:ws) = einfuegen w $ sortiere ws

waren = [Ware "krebs" 10.4 12, Ware "haus" 1000 1, Ware "waffe" 22 90]
