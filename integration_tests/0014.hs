data List a = Cons a (List a) | Nil

showList :: List String -> String
showList Nil = "[]"
showList (Cons x xs) = x ++ " : " ++ showList xs

-- OUT(A : B : C : [])
main :: String
main = showList (Cons "A" (Cons "B" (Cons "C" Nil)))
