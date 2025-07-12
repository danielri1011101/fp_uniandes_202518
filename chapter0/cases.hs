describe [] =  "The list is empty"
describe [x] = "The list is a singleton list"
describe lst = "The list is a longer list"


describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               x:[] -> "a singleton list."  
                                               xs -> "a longer list."  


compare [] [] = 0
compare [] _ = -1
compare _ [] = 1
compare (x:xs) (y:ys) =  compare xs ys

compare lst1 lst2 =  (len lst1) - (len lst2 )









zeros tuple = case tuple of
    (0, 0, 0) -> 3
    (0, 0, _) -> 2
    (0, _, 0) -> 2
    (_, 0, 0) -> 2
    (0, _, _) -> 1
    (_, 0, _) -> 1
    (_, _, 0) -> 1