{-
    Бинарное дерево задано в виде списка, в каждом узле хранится либо код операции (+-*/), либо число.
    Посчитать значение выражения, заданного деревом.
-}

module Task06 where

data Tree v = Leaf v | Mul (Tree v) (Tree v) | Div (Tree v) (Tree v) | Plus (Tree v) (Tree v) | Minus (Tree v) (Tree v)

calc (Mul left right) = calc left * calc right
calc (Div left right) = calc left / calc right
calc (Plus left right) = calc left + calc right
calc (Minus left right) = calc left - calc right
calc (Leaf value) = value

task06start = do
    let tree = Div (Plus (Plus (Plus (Mul (Plus (Leaf 5) (Leaf 3)) (Leaf 4)) (Div (Mul (Leaf 8) (Leaf 20)) (Leaf 10))) (Mul (Minus (Leaf 4) (Leaf 3)) (Leaf 8))) (Div (Leaf 8) (Mul (Leaf 9) (Leaf 4)))) (Leaf 10)
    calc tree