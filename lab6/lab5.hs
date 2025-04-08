sumPos ls = foldl op 0 ls
    where op x k = if k > 0 then x + k else x

lsAuto = [("subaru",-10,20, 15000), ("toyota", 1, 10, 10000), ("honda",-1, 24, 12000)]
sumPosAuto = foldl op 0 lsAuto
    where op x (_, k2, k3,k4) = if k2 > 0 then x + k3*k4 else x

sumPosAuto2 = foldl op (0, []) lsAuto
    where op (x,rls) (_, k2, k3,k4) = if k2 > 0 then (x + k3*k4,rls ++ [k3*k4]) else (x,rls)


evenSzor ls = foldl op 1 ls
    where op x k = if even k then x * k else x

foldn n = foldl op [] [1..n]
    where op rls k = k*k : rls

foldnr n = take n $ foldr op [] [1..]
    where op k rls = k*k : rls

polival x0 ls = foldr (op x0) 0 ls
    where op x0 r a  = x0*r + a

avgAuto ls = foldl op (0,0) ls
    where op (x,y) (_, k2, k3,k4) = if k2 > 0 then (x + k3*k4, y + k4) else (x,y)

toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

main :: IO ()
main = do
    print "Lab 6 solving lab 5"
    print $ sumPos [1,-22,-3,4,-25]
    print $ sumPosAuto
    print $ sumPosAuto2
    print $ evenSzor [1,2,3,4,5,6]
    print $ foldn 10
    print $ foldnr 10
    print $ polival 2 [1,3,4,2]
    print $ avgAuto lsAuto
    print $ toBin 2