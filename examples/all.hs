data A = A Int | B Int

increment :: A -> Int
increment (A x) = x + 1
increment (B x) = x + 2

bar :: Int -> Int
bar x = case x of
  3 -> 4
  5 -> 5
  _ -> x
  where
    x = 3
    y = 4

foo :: Int
foo = 
  let x = 3
  in increment (A x) + x

main = foo
