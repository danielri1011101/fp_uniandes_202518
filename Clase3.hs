zeepWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zeepWith f = foldr (cons . f $) []