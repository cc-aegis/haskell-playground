import Data.Maybe (catMaybes)
import Data.List ((!?))

data Cell = Paper | Empty deriving (Eq, Show)

type Grid a = [[a]]

parseCell :: Char -> Maybe Cell
parseCell '@' = Just Paper
parseCell '.' = Just Empty
parseCell _ = Nothing

parseGrid :: String -> Maybe (Grid Cell)
parseGrid = traverse (traverse parseCell) . lines
    
getXy :: Grid a -> Int -> Int -> Maybe a
getXy grid x y = do
    row <- grid !? y
    row !? x

neighbors :: Grid a -> Int -> Int -> [a]
neighbors grid x y = catMaybes [getXy grid x' y' | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]

notTooMuchPaper :: Grid Cell -> Int -> Int -> Bool
notTooMuchPaper grid x y = length (filter (==Paper) (neighbors grid x y)) < 4

indices :: Grid a -> [(Int, Int)]
indices rows = concatMap (\(y, row) -> fmap (\(x, _) -> (x, y)) (zip [0..] row)) (zip [0..] rows)

gridSpacesWithNotTooMuchPaper :: Grid Cell -> Int
gridSpacesWithNotTooMuchPaper grid = length $ filter (\ (x, y) -> notTooMuchPaper grid x y && getXy grid x y == Just Paper) $ indices grid

main :: IO ()
main = do
    input <- readFile "day4.txt"
    let grid = parseGrid input
    print $ fmap gridSpacesWithNotTooMuchPaper grid