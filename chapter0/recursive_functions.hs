squareRoot guess x =
   if goodEnough guess x 
    then do guess
    else do
        let guess1 = improve guess x 
        squareRoot guess1 x


sumSqr lst = 
        function lst 0
function [] accum = accum
function (x:xs) accum = function xs (accum+(x*x))


comp x n = if n == 0 then 1
    else x * comp (x) (n-1)


real n num =  if n == 0 then num
    else real (n-1) (num+1.0)


sorted [] = True
sorted (x:[]) = True 
sorted (x:(y:xs)) = x < y && sorted (y:xs) 

title str = " "
title (x:[]) = " "
title (x:(y:xs)) = if x == " "
    then append (upperCase y) (title xs)
    else append (upperCase x) (x:title xs)