apply :: (a -> b) -> a -> b
apply f x = f x

inc :: Int -> Int
inc x = x + 1

-- OUT(21)
main :: Int
main = apply inc 20
