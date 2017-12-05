data Operator = Plus | Minus
data Tree i o = Leaf i | Node (Tree i o) o (Tree i o)

tree :: Tree Int Operator
tree = Node (Leaf 1) Minus (Leaf 2)
-- tree = Node (Node (Leaf 20) Plus (Leaf 4)) Plus (Leaf 5)

flatten :: Tree Int Operator -> [Int]
flatten (Leaf x) = [x]
flatten (Node l _ r) = flatten l ++ flatten r

add :: Tree Int Operator -> Int
add (Leaf x) = x
add (Node x Plus y) = add x + add y
add (Node x Minus y) = add x - add y


calc :: String -> Double
calc x = 1