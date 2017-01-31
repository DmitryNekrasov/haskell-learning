module Task12 where

data Tree key height = Leaf | Node key height (Tree key height) (Tree key height)

height Leaf = 0
height (Node _ height _ _) = height

balanceFactor (Node _ _ left right) = height right - height left

fixHeight (Node key _ left right) = (Node key ((max (height left) (height right)) + 1) left right)

rotateRight (Node pKey pHeight (Node qKey qHeight qLeft qRight) pRight) =
    fixHeight (Node qKey qHeight qLeft newQRight)
    where newQRight = fixHeight (Node pKey pHeight qRight pRight)

rotateLeft (Node qKey qHeight qLeft (Node pKey pHeight pLeft pRight)) =
    fixHeight (Node pKey pHeight newPLeft pRight)
    where newPLeft = fixHeight (Node qKey qHeight qLeft pLeft)

utilityRight (Node key height left right) rFactor | rFactor < 0 =
    (Node key height left (rotateRight right))
utilityRight node _ = node

utilityLeft (Node key height left right) lFactor | lFactor > 0 =
    (Node key height (rotateLeft left) right)
utilityLeft node _ = node

balance2 (Node key height left right) diff
    | diff == 2 = rotateLeft (utilityRight (Node key height left right) (balanceFactor right))
    | diff == -2 = rotateRight (utilityLeft (Node key height left right) (balanceFactor left))
balance2 node _ = node

balance node =
    balance2 (fixHeight node) (balanceFactor node)

insert Leaf key = Node key 1 Leaf Leaf
insert (Node nodeKey nodeHeight left right) key | key < nodeKey =
    balance (Node nodeKey nodeHeight (insert left key) right)
insert (Node nodeKey nodeHeight left right) key =
    balance (Node nodeKey nodeHeight left (insert right key))

insertKeys node [] = node
insertKeys node (h : t) = insertKeys (insert node h) t

makeTree keys = insertKeys Leaf keys

toString Leaf = "[]"
toString (Node key height left right) =
    "[{" ++ (show key) ++ "," ++ (show height) ++ "}," ++ (toString left) ++ "," ++ (toString right) ++ "]"

task12start = do
    toString tree
    where tree = makeTree [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]