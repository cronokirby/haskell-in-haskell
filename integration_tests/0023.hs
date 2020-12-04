data List a = Cons a (List a) | Nil

concat :: List a -> List a -> List a
concat Nil xs = xs
concat (Cons x xs) ys = Cons x (concat xs ys)

cycle :: List a -> List a
cycle xs = xs2 where xs2 = concat xs xs2

at :: Int -> List a -> a
at 0 (Cons x _) = x
at n (Cons _ xs) = at (n - 1) xs

-- OUT(1)
main :: Int
main = at 100 $ cycle (Cons 1 (Cons 2 Nil)) 
