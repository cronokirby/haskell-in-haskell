data List a = Cons a (List a) | Nil

data Pair a b = Pair a b

get :: Int -> List a -> a
get 0 (Cons x _) = x
get n (Cons _ xs) = get (n - 1) xs

zipWithIndex :: List a -> List (Pair Int a)
zipWithIndex = go 0
  where
    go _ Nil = Nil
    go n (Cons x xs) = Cons (Pair n x) (go (n + 1) xs)

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

factorial :: Int -> Int
factorial n = get n facs
  where
    foo :: Pair Int Int -> Int
    foo (Pair i f) = (i + 1) * f

    facs :: List Int
    facs = Cons 1 (map foo (zipWithIndex facs))

-- OUT(522547200)
main :: Int
main = factorial 10 + factorial 11 + factorial 12
