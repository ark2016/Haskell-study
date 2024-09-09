module Zeros where
--подсчёт нулей в конце записи факториала числа
zeros :: Int -> Int
zeros x = sum [(div x n) | n <- take (floor (logBase 5 (fromIntegral x))) (map (5^) [1..])]
