
lookupF :: Eq k => k -> [(k,v)] -> (v -> a) -> Maybe a
lookupF = \ x ps f -> fmap f $ lookup x ps

-- Look up a value and a function, and apply if both are found.
lookupA :: Eq k => k -> [(k,v)] -> [(k, v -> a)] -> Maybe a
lookupA k0 vs fs = let (mf, mv) = (lookup k0 fs, lookup k0 vs)
                   in mf <*> mv

-- 6. Rose trees.
data Rose a = Nil | Rose (Leaved a)
data Leaved a = Root a [Leaved a]

-- Problematic. How to fix?

instance Functor Leaved where
    fmap f (Root x []) = Root (f x) []
    fmap f (Root x (lvd:lvds)) =
        Root (f x) (fmap f lvd : map (fmap f) lvds)

instance Functor Rose where
    fmap f Nil = Nil
    fmap f (Rose lvd) = Rose $ fmap f lvd