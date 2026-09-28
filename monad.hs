class Monad m where
    return :: a -> m a
    (>>) :: m a -> m b -> m b
    (>>=) :: m a -> (a -> m b) -> m b

data Option a = Some a | None

instance Monad (Option a) where
    return a = Some a

    a >> b = b

    None >>= f = None
    (Some a) >>= f = f a


main :: IO ()
main = do
    print "Hello World"