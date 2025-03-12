szamjegyszorzat n
    | n < 10 = n
    | otherwise = (n `mod` 10) * szamjegyszorzat (n `div` 10)

szamjegyszorzat2 n = aux n 1
    where aux n x 
            | n < 10 = x * n
            |otherwise = aux re (digit * x)
                where
                    digit = n `mod` 10
                    re = n `div` 10

szamjegyszam n
    | n < 10 = 1
    | otherwise = 1 + szamjegyszam (n `div` 10)


fugv4 :: Int -> Int -> Int
fugv4 n x 
    | n < 10 = if n == x then n else 0
    | otherwise = fugv4 (n `div` 10) x + num
        where 
            nu = n `mod` 10
            num = if nu == x then nu else 0

fib 0=0
fib 1=1
fib n = fib (n-1) + fib (n-2)

fib1k :: Integral a => a -> a
fib1k n = aux 0 1 n
    where
        aux :: Integral a => a -> a -> a -> a
        aux a b n
            | n == 0 = a
            | otherwise = aux b (a+b) (n-1)

main = do
    print ("lab 4")
    print $ szamjegyszorzat2 13123
    print $ szamjegyszam 123456
    print $ fugv4 77231472472347 7
    print $ fib1k 1000
    -- print $ fib 35