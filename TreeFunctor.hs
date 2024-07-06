import Data.Functor
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f <$> x)
    fmap f (Branch a b c) = Branch (f <$> a) (f <$> b) (f <$> c)