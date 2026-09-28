-- y :: (a -> a) -> a
y f = g g
    where g = \ x -> f $ x x

fac_rec rec n
    | n == 0 = 1
    | otherwise = n * rec (n - 1)

fac = y fac_rec

main :: IO ()
main = do
    print $ fac 5
