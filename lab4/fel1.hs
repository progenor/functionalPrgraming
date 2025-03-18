list1 n =  [  i*i | i <- [1..n], even i]
list12 n = take n [  i*i | i <- [2,4..]]
list13 n = take n [i*i | i <- iterate (+2) 2]

myiterate fg x = x : myiterate fg (fg x)


aux i = take i [i,i..]

list2 n = take n $ concat $ map aux [1..]
list2other n = take n $ concatMap aux [1..]

-- same as list 2 just with hardcode
mainList n = auxList n 1 1 1
    where
    auxList n i j k
        | n == 0 = []
        | j == k  = i : auxList (n-1) (i+1) 1 k+1
        | j < k = i : auxList (n-1) i (j+1) k


list5 n = take n $ cycle [True, False]

osztokszama n =  [i | i <- [1..n], n `mod` i == 0]
osztokszama2 n =  [ i | i <- [1..n],  n `mod` i == 0 && even i]

lnparatlano num 
    | mod num eredmeny == 0 = eredmeny
    | mod num 2 == 0 = 1
    | otherwise = num
        where
        eredmeny = lnparatlano' num 3

        lnparatlano' num k
            | div num 2 < k = 3
            | mod num k ==0 && maxi > k = maxi
            | mod num k ==0 && maxi <= k = k
            | otherwise = maxi
                where
                maxi = lnparatlano' num (k+1)


main = do
    -- print $ list1 10
    -- print $ list12 10
    -- print $ list13 10
    -- print  $ take 10 $ myiterate (+2) 2
    -- print $ list2 10
    -- print $ mainList 10
    print  "asd"
    -- print $ list5 10
    print $ osztokszama 10
    print $ osztokszama2 10
    print $ lnparatlano 128

