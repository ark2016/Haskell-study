{-
Воспользовавшись функциями map и concatMap, определите функцию perms, которая возвращает все перестановки,
которые можно получить из данного списка, в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка.
-}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insertEverywhere x) (perms xs)
  where
    insertEverywhere y [] = [[y]]
    insertEverywhere y (z:zs) = (y:z:zs) : map (z:) (insertEverywhere y zs)
