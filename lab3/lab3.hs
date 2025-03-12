list = [1,2,3,4,5,6,7,8,9,10]

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength2 :: [a] -> Int
myLength2 xs = sum [1 | _ <- xs]


main = do
    print "lab3"
    print $ myLength list
    print $ myLength2 list