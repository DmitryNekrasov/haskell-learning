module Task13 where

binPow _ 0 = 1
binPow a n | mod n 2 == 0 = do
    let b = binPow a (div n 2)
    b * b
binPow a n = a * binPow a (n - 1)

binPowList [] [] = []
binPowList (h1:t1) (h2:t2) = [binPow h1 h2] ++ binPowList t1 t2

task13start = do
    let a = [2, 3, 7, 15, 1, 4, 7]
    let b = [3, 4, 8, 5, 100000000000000000000, 0, 30]
    binPowList a b