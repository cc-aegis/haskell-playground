import Text.Read (readMaybe)
import Data.Traversable (traverse)

trimUntilFirstDigit :: String -> Maybe String
trimUntilFirstDigit [] = Nothing
trimUntilFirstDigit [_] = Nothing
trimUntilFirstDigit [x, y] = Just [x, y]
trimUntilFirstDigit (x:xs) = case trimUntilFirstDigit xs of
    Just (y:ys) -> Just $ if x >= y then (x:xs) else (y:ys)
    otherweise -> Nothing

maxWithCandidate :: Ord a => a -> [a] -> a
maxWithCandidate candidate [] = candidate
maxWithCandidate candidate (y:ys) = if candidate > maxOfRest then candidate else maxOfRest
    where maxOfRest = maxWithCandidate y ys

takeMaxDigits :: String -> Maybe (Char, Char)
takeMaxDigits str = case trimUntilFirstDigit str of
    Just (x:y:ys) -> Just (x, maxWithCandidate y ys)
    _ -> Nothing

joltage :: String -> Maybe Int
joltage str = do
    (lhs, rhs) <- takeMaxDigits str
    parsed_lhs <- readMaybe [lhs] :: Maybe Int
    parsed_rhs <- readMaybe [rhs] :: Maybe Int
    Just $ parsed_lhs * 10 + parsed_rhs

main :: IO ()
main = do
    input <- readFile "day3.txt"
    let joltages = traverse joltage $ words input
    print $ fmap sum joltages