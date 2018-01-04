module Intro where
import Data.Char

{-
    Функция, принимающая на вход два символа и возвращающая число,
    составленное из этих символов, если оба символа числовые, и 100 в противном случае.
-}

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then convertToInt x y else 100
    where
        convertToInt x y = (ord x - ord '0') * 10 + ord y - ord '0'

{------------------------------------------------------------------------------------------}


{-
    Зададим точки на плоскости парами типа (Double, Double).
    Функция, которая возвращает расстояние между двумя точками, передаваемыми ей в качестве аргументов.
-}

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt (pointSquare (fst p1, fst p2) + pointSquare (snd p1, snd p2))
    where
        pointSquare point = uncurry (-) point ^ 2

{------------------------------------------------------------------------------------------}


{-
    Функция, возвращающая двойной факториал
    Предполагается, что аргумент функции может принимать только неотрицательные значения.
-}

doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = n * doubleFact (n - 2)

{------------------------------------------------------------------------------------------}

{-
    Числа Фибоначчи на всей последовательности целых чисел
-}

fibonacci :: Integer -> Integer
fibonacci n | n > 0 = fib 1 n
            | otherwise = fib (-1) n
    where
        fib = fib' 0 1
        fib' acc prev p n | n == 0 = acc
        fib' acc prev p n = fib' (acc * p + prev) acc p (n - 1 * p)

{------------------------------------------------------------------------------------------}

{-
    N-ный элемент рекуррентной последовательности:
        a(0) = 1;
        a(1) = 2;
        a(2) = 3;
        a(k+3) = a(k+2) + a(k+1) − 2a(k)
-}

seqA :: Integer -> Integer
seqA n | n >= 0 && n <= 2 = n + 1
       | otherwise = seqA' 1 2 3 n
    where
        seqA' a0 a1 acc n | n == 2 = acc
                          | otherwise = seqA' a1 acc (acc + a1 - 2*a0) (n - 1)

{------------------------------------------------------------------------------------------}


{-
    Cумма и количество цифр десятичной записи заданного целого числа
-}

sumNCount :: Integer -> (Integer, Integer)
sumNCount x | x == 0 = (0, 1)
            | x < 0 = (getSum 0 (-x), getCount 0 (-x))
            | otherwise = (getSum 0 x, getCount 0 x)
    where
        getSum acc 0 = acc
        getSum acc x = getSum (acc + (x `mod` 10)) (x `div` 10)
        getCount acc 0 = acc
        getCount acc x = getCount (acc + 1) (x `div` 10)

{------------------------------------------------------------------------------------------}

{-
    Функция, находящая значение определенного интеграла
    от заданной функции f на заданном интервале [a,b] методом трапеций
-}

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = undefined

{------------------------------------------------------------------------------------------}

