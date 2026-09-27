import Data.Semigroup

-- data Ranged a = Ranged (Min Int) (Max Int) a
--
-- instance Functor Ranged where
--   fmap f (Ranged start end x) = Ranged start end $ f x
--
-- instance Applicative Ranged where
--   pure a = Ranged (pure ) (pure) a
--   Ranged start end f <*> Ranged start' end' x = Ranged (min start start') (max end end') $ f x
--
-- instance Monad Ranged where
--   (>>=) = _
--   (>>) = _




-- type Ranged a = (Min Int, Max Int, a)
--
-- parseIdent :: [Ranged Char] -> (Ranged String, [Ranged Char])
-- parseIdent [] = (pure [], [])
-- parseIdent (c:cs) = do
--     c' <- c
--     let (ident, rest) = parseIdent cs
--     ident' <- ident
--     (return (:) <$> c <*> )





-- GOAL (?): c :: Ranged Char, cs :: Ranged String, `(:) <$> c <*> cs` :: Ranged String; doesn't really need to be a monad ig

data Ranged a = Ranged { start :: Min Int, end :: Max Int, content :: a }
    deriving (Show)

instance Functor Ranged where
    fmap f (Ranged { start, end, content }) = Ranged { start, end, content = f content }

instance Applicative Ranged where
    pure content = Ranged { start = mempty, end = mempty, content }
    Ranged { start, end, content = f } <*> Ranged { start = start', end = end', content } = Ranged { start = start <> start', end = end <> end', content = f content }

getContent :: Ranged a -> a
getContent Ranged { content } = content

isAlphabetic :: Char -> Bool
isAlphabetic c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

parseIdent :: [Ranged Char] -> (Ranged String, [Ranged Char])
parseIdent [] = (pure [], [])
parseIdent (c:cs)
    | isAlphabetic $ getContent c = ((:) <$> c <*> cs', rest)
    | otherwise = (pure [], (c:cs))
        where (cs', rest) = parseIdent cs

main = do
    let src = (\(i, c) -> Ranged { start = pure i, end = pure i, content = c}) <$> zip [0..] "hello world"
    print $ parseIdent src
