wahr = \ t f -> t
falsch = \ t f -> f
und p q = p q falsch

-- und wahr falsch
--     = wahr falsch falsch
--     = falsch

oder p q = p wahr q
nicht p = flip p
nicht' p = p falsch wahr


f :: Num a => (Bool, a) -> a
f (x, y) = if x then y + 1 else 0

g :: (a -> a) -> a -> a
g h x = h (h x)
-- g = \ h -> \ x -> h (h x)

k :: a -> b -> a
k = \ x y -> x
k' x y = x

k'' :: (a, b) -> a
k'' (a, b) = a
