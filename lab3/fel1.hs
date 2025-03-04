-- -- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?
-- atlag :: (Floating a) => [a] -> a
-- atlag ls = (sum ls) / fromIntegral (length ls)

--     > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
--     > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
--     > (take 4 . reverse . filter odd ) [1..20]
--     > take 4 . reverse . filter odd $ [1..20]
--     > take 4 ( reverse ( filter odd [1..20]))
--     > take 4 $ reverse $ filter odd $ [1..20]


-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?
atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

main :: IO ()
main = do
    putStrLn "Expression 1: (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]"
    print $ (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
    
    putStrLn "\nExpression 2: atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]"
    print $ atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
    
    putStrLn "\nExpression 3: (take 4 . reverse . filter odd) [1..20]"
    print $ (take 4 . reverse . filter odd) [1..20]
    
    putStrLn "\nExpression 4: take 4 . reverse . filter odd $ [1..20]"
    print $ take 4 . reverse . filter odd $ [1..20]
    
    putStrLn "\nExpression 5: take 4 (reverse (filter odd [1..20]))"
    print $ take 4 (reverse (filter odd [1..20]))
    
    putStrLn "\nExpression 6: take 4 $ reverse $ filter odd $ [1..20]"
    print $ take 4 $ reverse $ filter odd $ [1..20]