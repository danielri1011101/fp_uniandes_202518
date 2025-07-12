
factRF n = if n == 0
    then 1
    else n * factRF (n-1)


factRP n = factIter n 1
factIter n accum = if n == 0
    then accum
    else factIter (n-1) (n*accum)


main = putStrLn . show $ factRP 5
