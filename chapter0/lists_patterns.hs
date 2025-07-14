module ListsPatterns
( len,
   nth,
   concatenate,
) where

len lst = lenIter lst 0
lenIter [] accum = accum
lenIter lst accum = lenIter (tail lst) (1 + accum)

nth [] index =  -1
nth (head:tail) 0 = head
nth lst index = nth (tail lst) (index - 1)

concatenate first second = concatIter first second []
concatIter [] [] res = reverse res
concatIter [] second res = concatIter [] (tail second) ((head second):res)
concatIter first second res = concatIter (tail first) second ((head first):res)

firstAndThird (x:_:z:_) = "the elements are " ++ show x ++ " and " ++ show x
firstAndThird _ = "Not three elements"