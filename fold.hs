foldl' :: (a -> b -> b) -> b -> [a] -> b
foldl' _ base [] = base
foldl' combine base (x:xs) = combine x (foldl' combine base xs)

foldr' :: (b -> a -> b) -> b -> [a] -> b
foldr' _ base [] = base
foldr' combine base (x:xs) = combine (foldr' combine base xs) x

main :: IO ()
main = do
    let nums = ["1", "2", "3"]
    print $ foldl' (++) "" nums
    print $ foldr' (++) "" nums