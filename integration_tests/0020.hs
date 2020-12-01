data List a = Cons a (List a) | Nil

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

showList :: List String -> String
showList Nil = "[]"
showList (Cons x xs) = x ++ " : " ++ showList xs

-- OUT(A? : B? : [])
main :: String
main = showList (map (\s -> s ++ "?") (Cons "A" (Cons "B" Nil)))
