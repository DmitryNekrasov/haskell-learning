{-
    Дано число N. Сгенерировать список из цифр числа.
-}

module Task01 where

makeList 0 = []
makeList n = makeList(div n 10) ++ [mod n 10]

task01start = makeList(1234567)
