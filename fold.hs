foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ base [] = base
foldl' combine base (x:xs) = foldl' combine (combine base x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ base [] = base
foldr' combine base (x:xs) = combine x (foldr' combine base xs)

main :: IO ()
main = do
    let nums = ["1", "2", "3"]
    let join a b = "(" ++ a ++ ", " ++ b ++ ")"
    print $ foldl join "b" nums
    print $ foldl' join "b" nums
    print $ foldr join "b" nums
    print $ foldr' join "b" nums