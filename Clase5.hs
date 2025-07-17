-- Reimplementing Functor ideas

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f = \ mx -> case mx of
    Just x -> Just $ f x
    Nothing -> Nothing

class Functor f => Aplicative1 f where
    pure1 :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b

-- Usual lists define a functor by the standard prelude 
instance Aplicative1 [] where
    pure1 = \ x -> [x]
    fs <**> xs = [f x | f <- fs, x <- xs]

newtype ZipList1 a = ZipList1 {getIt :: [a]}

instance Functor ZipList1 where
    fmap f = ZipList1 . fmap f . getIt

instance Aplicative1 ZipList1 where
    pure1 = ZipList1 . repeat
    zl_fs <**> zl_xs = ZipList1 $ zipWith ($) (getIt zl_fs) (getIt zl_xs)

-- Now using the standard Applicative to instantiate Wolf's Logged data
-- type.

data Logged a = L a String

instance Functor Logged where
    fmap f (L x msg) = L (f x) msg

instance Applicative Logged where
    pure = flip L ""
    -- Point free for pure x = L x ""
    (L f mf) <*> (L x mx) = L (f x) (mf ++ "\n" ++ mx)

--
-- Exercises
--

data Via = Kr | Cl deriving Eq

-- Colombian address: tipo-de-vía, primer número, primer sufijo,
-- segundo número, segundo sufijo, número de metros desde inicio de
-- cuadra, detalles.
data CAddress = CAddress {
                          via :: Via, pNr :: Int, pSf :: String,
                          sNr :: Int, sSf :: String, mts :: Int,
                          ds :: String
                         }

instance Show CAddress where
    show x
      | via x == Kr = "Carrera " ++ showTail
      | via x == Cl = "Calle " ++ showTail
      where
        showTail = nCra ++ " - " ++ (show $ mts x) ++ " " ++ ds x
        nCra = show (pNr x) ++ pSf x ++ " # " ++  show (sNr x) ++ sSf x

home = CAddress {
                 via = Kr, pNr = 2, pSf = "", sNr = 16, sSf = "A", mts = 38,
                 ds = "Torre 7 Apartamento 405"
                }

ac1 = putStrLn "Hello"
ac2 = putStrLn "World"
ac3 = do {_ <- ac1; ac2}

combine :: IO a -> IO b -> IO b
combine ax ay  = do
    ax
    ay

-- _do_ is syntactic sugar for successive binding.

-- What should be the type of _main_?
main = do
    putStrLn "Hola!"
    return 3
    return ()

when1 :: Bool -> IO () -> IO ()
when1 = \ b ac -> if b then ac else return ()