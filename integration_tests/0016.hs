ifThenElse True a _ = a
ifThenElse False _ b = b

loop = loop

-- OUT(No Loop)
main = ifThenElse True "No Loop" loop
