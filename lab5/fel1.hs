
ls = [ "function", "class", "Float", "higher-order", "monad", "tuple", "variable", "Maybe", "recursion" ]

lenghts ls = map length ls

lengthsstr = [ (x, length x) | x <- ls]

minLengthStr  = minimum  lengthsstr


talalat elem ls i rls
    | null ls = rls
    | elem == k = talalat elem ve  (i+1) i++rls
    | otherwise = talalat elem ve  (i+1) rls 
    where
        k = head ls
        ve = tail ls


talalat2 elem ls = rls
    where
        zls = zip ls [0..]
        rls = [(x,y) | (x,y) <- zls, x == elem]

average x = sum x / fromIntegral (length x)

myprint (x1, x2) = putStrLn $ x1 ++ " " ++ show x2


myzip2 ls1 ls2 = zip ls1 ls2
myzip (x1,x2,x3) x4 = (x1,x2,x3,x4)

main = do
    print "lab5 "
    -- print $ lenghts ls
    -- print $ lengthsstr 
    print $ minLengthStr 
    -- let x = talalat ('e' "sziaaa" 0 [])
    -- print x
