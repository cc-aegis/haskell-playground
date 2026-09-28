import Text.Read (readMaybe)
import Data.Traversable (traverse)

data DialRotation = LeftRot Int | RightRot Int

readInt :: String -> Maybe Int
readInt num = readMaybe num :: Maybe Int

parseDialMovement :: String -> Maybe DialRotation
parseDialMovement ('L':num) = fmap LeftRot $ readInt num
parseDialMovement ('R':num) = fmap RightRot $ readInt num
parseDialMovement _ = Nothing

parseDialMovements :: String -> Maybe [DialRotation]
parseDialMovements dials = traverse parseDialMovement $ words dials

applyDialMovement :: Int -> DialRotation -> Int
applyDialMovement before (LeftRot rot) = before - rot
applyDialMovement before (RightRot rot) = before + rot

countDialZeros :: Int -> [DialRotation] -> Int
countDialZeros _ [] = 0
countDialZeros rot (mov:movs) =
    let afterRotation = (applyDialMovement rot mov) `mod` 100 in
        let remainingDialZeros = countDialZeros afterRotation movs in
            remainingDialZeros + if afterRotation == 0 then 1 else 0

countDialPassings :: Int -> [DialRotation] -> Int
countDialPassings _ [] = 0
countDialPassings rot (mov:movs) =
    let afterRotation = (applyDialMovement rot mov) in
        let remainingDialPassings = countDialPassings (afterRotation `mod` 100) movs in
            remainingDialPassings + (max 0 ((max 0 (afterRotation `div` 100)) + (max 0 ((100 - afterRotation) `div` 100)) - if afterRotation == 0 then 1 else 0))

main :: IO ()
main = do
    raw <- readFile "day1.txt"
    let dialMovements = parseDialMovements raw
    print $ fmap (countDialZeros 50) dialMovements
    print $ fmap (countDialPassings 50) dialMovements