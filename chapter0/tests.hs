len lst = lenIter lst 0

lenIter :: [Int] -> [Int] - [Int]
lenIter [] accum = accum
lenIter (x:xs) accum =  lenIter xs (1 + accum)



concat lst1 lst2 = 
    if lst1 == []
        then lst2
        else concat (init lst1) ((last lst1):lst2)

contact lst1 lst2 = superContoncat (reverse lst1) lst2
    if lst1 == []
        then lst2
        else superConcat (tail lst1) ((head lst1):lst2)
