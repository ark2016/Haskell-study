module Palindrome where
{-
Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.

GHCi> isPalindrome "saippuakivikauppias"
True
GHCi> isPalindrome [1]
True
GHCi> isPalindrome [1, 2]
False
-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = poli xs where
    poli [] = True
    poli [_] = True
    poli xs = head xs == last xs && poli (tail (init xs))



