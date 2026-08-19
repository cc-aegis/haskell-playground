split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:l) =
    let (lhs, rhs) = split l in
        (x:lhs, y:rhs)

merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x:lhs) (y:rhs) =
    if x < y then
        x:(merge lhs (y:rhs))
    else
        y:(merge (x:lhs) rhs)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort l =
    let (lhs, rhs) = split l in
        merge (mergesort lhs) (mergesort rhs)

main :: IO ()
main = do
    print (mergesort [12, 24, 0, -4, 15, 6])