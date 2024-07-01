module Sumncount where
{-
Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = undefined

GHCi> sum'n'count (-39)
(12,2)
-}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sum, count) where
  (sum, count) = if x == 0 then (0, 1) else sum' (abs x) 0 0
  sum' x s c = if x == 0 then (s, c) else sum' (div x 10) (s + mod x 10) (c + 1)
