import Data.Char

data Operator = Plus | Minus | Multiplication | Division
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

parse :: String -> [String]
parse (s:xs) | isDigit s = [parseNumber (s:xs)] ++ parse (dropWhile isDigit xs)
			 | isSymbol s = [showOperator (parseOperator (s:xs))] ++ parse xs
             | otherwise = []

parseNumber :: String -> String
parseNumber s = takeWhile isDigit s

parseOperator :: String -> Operator
parseOperator (s:xs) | s == '+' = Plus
                     | s == '-' = Minus
                     | s == '*' = Multiplication
                     | s == '/' = Division

showOperator :: Operator -> String
showOperator Plus = "+"
showOperator Minus = "-"
showOperator Multiplication = "*"
showOperator Division = "/"