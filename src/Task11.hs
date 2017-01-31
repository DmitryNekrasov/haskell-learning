module Task11 where

myGcd a 0 = a
myGcd a b = myGcd b (mod a b)

myGcdList (h:[]) = h
myGcdList (h:t) = myGcd h (myGcdList t)

task11start = myGcdList [150, 45, 90, 105, 24]