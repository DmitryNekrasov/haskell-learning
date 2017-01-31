module Task03 where

bubbleSwap [] = []
bubbleSwap (h1 : h2 : t) | h1 > h2 = (h2 : bubbleSwap (h1 : t))
bubbleSwap (h : t) = (h : bubbleSwap t)

bubbleSortU a 0 = a
bubbleSortU a n = bubbleSortU (bubbleSwap a) (n - 1)

bubbleSort a = bubbleSortU a (length a)

task03start = bubbleSort [5, 4, 3, 2, 1, 2, 3, 4, 3, 5, 4, 2, 3, 7]