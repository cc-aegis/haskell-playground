subdivide :: (a -> Bool) -> [a] -> ([a], [a])
subdivide _ [] = ([], [])
subdivide cond (x:xs)
    | cond x = (x:lhs, rhs)
    | otherwise = (lhs, x:rhs)
        where (lhs, rhs) = subdivide cond xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lhs) ++ [x] ++ (quicksort rhs)
    where (lhs, rhs) = subdivide (\e -> e < x) xs

main :: IO ()
main = do
    print $ quicksort [5, 1, 8, 2, 6, 3]