fibs = 0 : 1 : (uncurry (+) <$> zip fibs (drop 1 fibs))
fibs' = 0 : 1 : zipWith (+) fibs (tail fibs)

main = do
    print $ take 10 fibs
    print $ take 10 fibs'
