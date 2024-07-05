{-
Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле, что существуют
взаимно обратные функции, преобразующие List a в [a] и обратно. Реализуйте эти функции.
-}
data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a ax) = a : (fromList ax)

toList :: [a] -> List a
toList [] = Nil
toList (a : ax) = Cons a (toList ax)