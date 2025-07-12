pow x n = if n == 0 
    then 1
    else x*pow (x) (n-1)

powIter x n accum = if n == 0 then accum
    else powIter x (n-1) (x * accum)