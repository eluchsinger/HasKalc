data Operator = Plus | Minus
data Tree i o = Leaf i | Node (Tree i o) o (Tree i o)

tree :: Tree Double Operator
tree = Node (Leaf 1) Minus (Leaf 2)

flatten :: Tree Double Operator -> [Double]
flatten (Leaf x) = [x]
flatten (Node l _ r) = flatten l ++ flatten r

parse :: String -> String
parse s = (takeWhile isDigit s)

main:: IO()
main = do
   putStrLn ("Enter your calculation: ");
   line <- getLine;
   do
      arr <- parse line;
      putStrLn (line)