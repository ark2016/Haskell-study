module seqA where
{-
Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности
a_0 = 1; a_1 = 2; a_2 = 3; a_{k+3} = a_{k+2} + a_{k-1} - 2 a_{k}
Попытайтесь найти эффективное решение.

GHCi> seqA 301
1276538859311178639666612897162414
-}
seqA :: Integer -> Integer
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = helper 3 2 1 n

helper :: Integer -> Integer -> Integer -> Integer -> Integer
helper n3 _ _ 2 = n3
helper n3 n2 n1 n = helper (n3+n2 - 2*n1) n3 n2 (n-1)
