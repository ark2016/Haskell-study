{-
Составьте список сумм соответствующих элементов трех заданных списков. Длина результирующего списка должна быть 
равна длине самого длинного из заданных списков, при этом «закончившиеся» списки не должны давать вклада в суммы.

GHCi> sum3 [1,2,3] [4,5] [6]
[11,7,3]
-}
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 (a:as) (b:bs) (c:cs) = (a + b + c) : sum3 as bs cs
sum3 (a:as) (b:bs) [] = (a + b) : sum3 as bs []
sum3 (a:as) [] (c:cs) = (a + c) : sum3 as [] cs
sum3 [] (b:bs) (c:cs) = (b + c) : sum3 [] bs cs
sum3 (a:as) [] [] = a : sum3 as [] []
sum3 [] (b:bs) [] = b : sum3 [] bs []
sum3 [] [] (c:cs) = c : sum3 [] [] cs
