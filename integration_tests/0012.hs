data Apply a b = Apply (a -> b)

apply :: Apply a b -> a -> b
apply (Apply f) x = f x

-- OUT(21)
main :: Int
main = apply (Apply (\x -> x + 1)) 20
