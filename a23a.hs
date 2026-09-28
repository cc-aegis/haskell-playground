data Citizen = Kenan | Leon;

isFirstClassCitizen :: Citizen -> Bool
isFirstClassCitizen Kenan = False
isFirstClassCitizen _ = True

main :: IO ()
main = do
    print $ isFirstClassCitizen Kenan