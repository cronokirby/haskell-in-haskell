-- OUT(1111)

foo 0 0 = 1
foo 1 1 = 10
foo 2 2 = 100
foo _ _ = 1000

main = foo 0 0 + foo 1 1 + foo 2 2 + foo 3 3
