import Text.Read (readMaybe)
import Data.Traversable (traverse)
import Data.List.Split (splitOn)

digitsOfInt :: Integer -> Integer
digitsOfInt n
    | n < 10 = 1
    | otherwise = 1 + digitsOfInt (n `div` 10)

-- 2n-digit and divisible by (1+10^n) 
isInvalidId :: Integer -> Bool
isInvalidId n = (even digits) && (0 == mod n (1 + 10 ^ (div digits 2))) -- TODO: guards instead of &&
    where digits = digitsOfInt n

invalidIdsInRange :: (Integer, Integer) -> [Integer]
invalidIdsInRange (start, end)
    | start <= end && isInvalidId start = start:(invalidIdsInRange (start + 1, end))
    | start <= end = invalidIdsInRange (start + 1, end)
    | start > end = []

sumInvalidIdsInRange :: (Integer, Integer) -> Integer
sumInvalidIdsInRange = sum . invalidIdsInRange

sumInvalidIdsInRanges :: [(Integer, Integer)] -> Integer
sumInvalidIdsInRanges ranges = sum $ fmap sumInvalidIdsInRange ranges

zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe (Just x) (Just y) = Just (x, y)
zipMaybe _ _ = Nothing

parseRange :: String -> Maybe (Integer, Integer)
parseRange range = case splitOn "-" range of
    [start, end] -> zipMaybe (readMaybe start :: Maybe Integer) (readMaybe end :: Maybe Integer)
    _ -> Nothing

parseRanges :: String -> Maybe [(Integer, Integer)]
parseRanges ranges = traverse parseRange $ splitOn "," ranges

main :: IO ()
main = do
    rawRanges <- readFile "day2.txt"
    let maybeRanges = parseRanges rawRanges
    print $ fmap sumInvalidIdsInRanges maybeRanges