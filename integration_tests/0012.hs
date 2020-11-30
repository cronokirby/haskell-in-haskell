data Apply a b = Apply (a -> b)

apply :: Apply a b -> a -> b
apply (Apply f) x = f x

const :: a -> b -> a
const a _ = a

-- OUT(foo)
main :: String
main = apply (Apply (const "foo")) 20
