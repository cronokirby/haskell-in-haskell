data List a = Cons a (List a) | Nil

tail :: List a -> List a
tail Nil = Nil
tail (Cons _ rest) = rest

at :: Int -> List a -> a
at 0 (Cons x _) = x
at n (Cons _ xs) = at (n - 1) xs

fibs :: List Int
fibs = Cons 0 (Cons 1 (add fibs (tail fibs)))
  where
    add :: List Int -> List Int -> List Int
    add (Cons a as) (Cons b bs) = Cons (a + b) (add as bs)

fib :: Int -> Int
fib n = at n fibs

main :: Int
main = fib 100