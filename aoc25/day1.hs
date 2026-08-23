import Text.Read (readMaybe)
import Data.Traversable (traverse)

type Dial = Int

readInt :: String -> Maybe Int
readInt num = readMaybe num :: Maybe Int

parseDial :: String -> Maybe Dial
parseDial ('L':num) = fmap (100-) $ readInt num
parseDial ('R':num) = readInt num
parseDial _ = Nothing

parseDials :: String -> Maybe [Dial]
parseDials dials = traverse parseDial $ words dials

countDialZeros :: Int -> [Dial] -> Int
countDialZeros _ [] = 0
countDialZeros rot (dial:dials) =
    let afterRotation = (rot + dial) `mod` 100 in
        let remainingDialZeros = countDialZeros afterRotation dials in
            remainingDialZeros + if afterRotation == 0 then 1 else 0

main :: IO ()
main = do
    raw <- readFile "day1.txt"
    print $ fmap (countDialZeros 50) $ parseDials raw 
