data List a = Cons a (List a) | Nil

take :: Int -> List a -> List a
take _ Nil = Nil
take 0 _ = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

showList :: List String -> String
showList Nil = "[]"
showList (Cons x xs) = x ++ " : " ++ showList xs

repeat :: a -> List a
repeat a = Cons a (repeat a)

-- OUT(A : A : A : [])
main :: String
main = showList . take 3 $ repeat "A"
