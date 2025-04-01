import Data.Char
import Data.List
import Text.Printf
mFib n a b ls
    | n ==0 = ls
    | otherwise = mFib (n-1) b (a+b) (elsoSzamJegy a :ls)
    where
        elsoSzamJegy a = (head . show) a 

mFilter1 ls = filter (\x -> head ls == x) ls

mFilter2 ls = filter (\x -> head ls /= x) ls

mPower x n = [x^i | i <- [0..n]]





mainBenford n  =   valls
    where
        bls = auxBenford $ mFib n 0 1 []
        sortls = sortOn fst bls
        valls = map (\(x , y) -> (x, fromIntegral y / n))  sortls

auxBenford fibls
    | null fibls = []
    | otherwise = ( k , length f1) : auxBenford f2
        where
            f1 = mFilter1 fibls
            f2 = mFilter2 fibls
            k = digitToInt $ head fibls

-- mPrint (a, b)  = putStrLn $ show a ++ " : " ++ show b
mPrint (a, b) = do 
    putStr $ show a ++ " : " 
    printf "%.2f\n" b



main :: IO ()
main = do
    print "lab 5 continue"
    print $ mFib 20 0 1 []
    -- print $ mFilter1 $ mFib 20 0 1 []
    mapM_ print $ mainBenford 20