module Task04 where

merge a [] = a
merge [] a = a
merge (h1 : t1) (h2 : t2) | h1 < h2 = (h1 : merge t1 (h2 : t2))
merge (h1 : t1) (h2 : t2) = (h2 : merge (h1 : t1) t2)

mergeSort [] = []
mergeSort (h:[]) = [h]
mergeSort a = do
    merge (mergeSort . take half $ a) (mergeSort . drop half $ a)
    where half = div (length a) 2

task04start = mergeSort [4, 6, 5, 7, 1, 3, 2, 4, 5, 6, 7]