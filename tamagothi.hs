data Tamagotchi = Tamagotchi {
    happy :: Int,
    bored :: Int,
    hungry :: Int,
    tired :: Int
}

wait :: Tamagotchi -> Tamagotchi
wait (Tamagotchi happiness hunger tiredness) = Tamagotchi happiness (hunger + 1) (tiredness + 1)

sleep :: Tamagotchi -> Tamagotchi
sleep (Tamagotchi happiness hunger tiredness) = Tamagotchi happiness (hunger + 1) (tiredness - 2)

eat :: 

play ::