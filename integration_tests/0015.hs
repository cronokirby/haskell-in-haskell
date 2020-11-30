data List a = Cons a (List a) | Nil

reverse :: List a -> List a
reverse = go Nil
  where
    go base Nil = base
    go base (Cons x xs) = go (Cons x base) xs

showList :: List String -> String
showList Nil = "[]"
showList (Cons x xs) = x ++ " : " ++ showList xs

-- OUT(C : B : A : [])
main :: String
main = showList . reverse $ (Cons "A" (Cons "B" (Cons "C" Nil)))
