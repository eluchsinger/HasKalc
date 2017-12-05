import Data.Char

data Operator = Plus | Minus
data Tree i o = Leaf i | Node (Tree i o) o (Tree i o)

tree :: Tree Double Operator
tree = Node (Leaf 1) Minus (Leaf 2)
-- tree = Node (Node (Leaf 20) Plus (Leaf 4)) Plus (Leaf 5)

flatten :: Tree Double Operator -> [Double]
flatten (Leaf x) = [x]
flatten (Node l _ r) = flatten l ++ flatten r

add :: Tree Double Operator -> Double
add (Leaf x) = x
add (Node x Plus y) = add x + add y
add (Node x Minus y) = add x - add y

-- add2 :: Int -> Double
-- add2 x = fromIntegral x

-- groupTerms :: [Char] -> [[Char]]
-- groupTerms s = [x | x <- s, take]

-- parseInts :: String -> String
-- parseInts s = filter (isDigit) s

parse :: String -> [String]
parse [] = [""]
parse s = [(takeWhile isDigit s)] ++ [(take 1 (dropWhile isDigit s))] ++ (parse (drop 1 (dropWhile isDigit s)))

-- takeInts :: [Char] -> [Int]
-- takeInts s = [x | x <- s]

-- calc :: String -> Double
-- calc x = 1