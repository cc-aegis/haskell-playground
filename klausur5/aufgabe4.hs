data Befehl = Push Int | Add | Mul | Dup | Swap
    deriving Show

code :: [Befehl]
code = [Push 3, Push 10, Add, Push 7, Swap, Mul, Dup, Add]
-- 7 * (3 + 10) + 7 * (3 + 10) = 182

code' = [Push 2, Push 3, Add, Dup, Mul] -- 25
code'' = [Push 7, Push 2, Swap, Push 1, Add] -- 8, 2

schritt :: [Int] -> Befehl -> [Int]
schritt stack (Push x) = x : stack
schritt (x:y:stack) Add = (x + y) : stack
schritt (x:y:stack) Mul = (x * y) : stack
schritt (x:stack) Dup = x : x : stack
schritt (x:y:stack) Swap = y : x : stack
schritt _ _ = undefined

ausfuehren :: [Befehl] -> [Int]
ausfuehren = foldl schritt []

data Ausdruck = Zahl Int | Plus Ausdruck Ausdruck | Mal Ausdruck Ausdruck
    deriving Show

ausdruck :: Ausdruck
ausdruck = Plus (Mal (Zahl 3) (Zahl 1)) (Plus (Mal (Zahl 3) (Zahl 8)) (Zahl 5))
-- (3 * 1) + ((3 * 8) + 5) = 32

auswerten :: Ausdruck -> Int
auswerten (Zahl x) = x
auswerten (Plus lhs rhs) = sum $ map auswerten [lhs, rhs] -- auswerten lhs + auswerten rhs
auswerten (Mal lhs rhs) = auswerten lhs * auswerten rhs

uebersetze :: Ausdruck -> [Befehl]
uebersetze (Zahl x) = [Push x]
uebersetze (Plus lhs rhs) = uebersetze lhs ++ uebersetze rhs ++ [Add]
uebersetze (Mal lhs rhs) = uebersetze lhs ++ uebersetze rhs >:> Mul

(>:>) l x = l ++ [x] -- fish :D
