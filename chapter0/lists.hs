module Lists
( len,
   nth,
   concatenate,
) where

len lst = lenIter lst 0
lenIter lst accum = if lst == []
    then accum
    else lenIter (tail lst) (1 + accum)

nth lst index =  if lst == []
    then -1
    else if index == 0 
        then head lst
        else nth (tail lst) (index - 1)

concatenate first second = concatIter first second []
concatIter first second res = if first == [] && second == []
    then reverse res
    else if first == []
        then concatIter first (tail second) ((head second):res)
        else concatIter (tail first) second ((head first):res)