data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show

tree :: Tree Int
tree = Node (Node Empty 2 (Node Empty 3 Empty)) 4 (Node Empty 8 Empty)

size :: Tree a -> Int
size Empty = 0
size (Node lhs _ rhs) = 1 + (size lhs) + (size rhs)

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member v (Node lhs v' rhs)
    | v == v' = True
    | v < v' = member v lhs
    | v > v' = member v rhs

insert :: Ord a => a -> Tree a -> Tree a
insert v Empty = Node Empty v Empty
insert v (Node lhs v' rhs)
    | v == v' = Node lhs v' rhs
    | v < v' = Node (insert v lhs) v' rhs
    | v > v' = Node lhs v' (insert v rhs)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node lhs v rhs) = (inorder lhs ++) $ (:) v $ inorder rhs


aufgabe4a1 :: Num a => [a] -> [a]
aufgabe4a1 = map (*2)

aufgabe4a2 :: [a] -> [a]
aufgabe4a2 = foldr (:) []

aufgabe4a3 :: Num a => a -> a
aufgabe4a3 = flip (-) 1

aufgabe4a4 :: (a -> a) -> a -> a
aufgabe4a4 = \ f x -> f (f x)
-- aufgabe4a4 f x -> f (f x)

count :: (a -> Bool) -> [a] -> Int
count cond = foldr (\ v -> (+) $ if cond v then 1 else 0) 0

countEven :: [Int] -> Int
countEven = count even
