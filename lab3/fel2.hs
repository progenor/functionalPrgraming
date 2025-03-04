import Data.List
import Data.Char

lista1 n=[1..n]

lista2 n=[sqrt i | i<-[1..n]]

lista3 n=[i^2 | i<-[1..n]]

lista4 n=[i | i<-[1..n], test i]

test i = fromIntegral ( truncate (i^1/2)) /= sqrt i

lista4_ n = [1..n] \\ lista3 n

lista5 n=[(i, i^3) | i<-[1..n]]


lista6 x  n = [x^i | i<-[0..n]]


list7 n = [i| i<-[1..n], mod n i == 0, even 1]

list7_ n = filter even [i | i<-[1..n], mod n i == 0]

list8 n = filter even [ i | i <- [1..n], testPrim i 3] 

testPrim x m
    | m ^ 2 > x = True
    | x `mod` m == 0 = False
    | otherwise = testPrim x (m+2)

list9 n =  [ i | i <- [1..n], not $ testPrim2 i 3] 

testPrim2 x m
    | x == 2 = True
    | even x = False  
    | m ^ 2 > x = True
    | x `mod` m == 0 = False
    | otherwise = testPrim x (m+2)


list10 n = [1..n] \\ list8 n

list12 n = [(x, y,z) | x <- [1..n], y <- [1..n], z<-[y+1..n], x^2 == y^2 + z^2]


list13 n = [(x, y) | x <- ['a'..'z'], y <- [0..25], y == ord x -97]

list13_ = zip ['A'..'z'] [0..60]

list14 n = [(x, y) | x<-[0..n] , y <- [n, n-1..0], x+y == 5]

list15 n = [even x  | x <-  [1..n] ]


main :: IO()
main = do
    mapM_ print $ list15 20
    -- let ls8 = list8 60
    -- print (length ls8, ls8)