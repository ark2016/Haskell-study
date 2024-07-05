{-
Рассмотрим еще один пример рекурсивного типа данных:

data Nat = Zero | Suc Nat
Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее. 
Таким образом мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления.

Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1
Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.
-}
data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n  = (Suc (toNat (n - 1)))

add :: Nat -> Nat -> Nat
add a b = toNat (fromNat a + fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat (fromNat a * fromNat b)

fac :: Nat -> Nat
fac n = toNat (factorial (fromNat n))

factorial:: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n - 1)
