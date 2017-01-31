{-
    Найти все совершенные числа (число равно сумме своих делителей) , не превосходящие N.
-}

module Task02 where

getNumber 0 = []
getNumber n = (n : getNumber (n - 1))

getDividerList n = [x | x <- (getNumber (n - 1)), (mod n x) == 0]

getPerfectNumber n = [x | x <- reverse . getNumber $ n, x == (sum . getDividerList $ x)]

task02start = getPerfectNumber 10000