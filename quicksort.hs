import Data.List (partition)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lhs) ++ [x] ++ (quicksort rhs)
    where (lhs, rhs) = partition (<x) xs

main :: IO ()
main = do
    print $ quicksort [5, 1, 8, 2, 6, 3]