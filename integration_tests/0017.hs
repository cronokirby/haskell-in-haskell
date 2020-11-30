data List a = Cons a (List a) | Nil

head :: List a -> a
head (Cons x _) = x

loop = loop

-- OUT(No Loop)
main = head (Cons "No Loop" loop)