data List a = Cons a (List a) | Nil

sum :: List Int -> Int
sum Nil = 0
sum (Cons x xs) = x + sum xs

-- OUT(6)
main :: Int
main = sum (Cons 1 (Cons 2 (Cons 3 Nil)))
