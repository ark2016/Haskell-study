fibonacci :: Integer -> Integer
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | n == -1 = 1
    | n < 0  = if not (even n) then fibonacci (-n) else (-fibonacci (-n))
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
