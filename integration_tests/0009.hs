-- OUT(3628800)

fac 0 = 1
fac n = n * fac (n - 1)

main = fac 10
