import Data.List
import Data.Ord

konyvls = [
    ("Konyv 1", 90, [10, 5 ,7, 8]),
    ("Konyv 2", 50, [5, 20 ,2, 3, 12]),
    ("Konyv 3", 20, [2, 3 ,4, 5, 6]),
    ("Konyv 4", 70, [1, 2 ,3, 4, 5]),
    ("Konyv 5", 50, [1, 2 ,3, 4, 5]),
    ("Konyv 6", 90, [1, 2 ,3, 4, 5])]

rendez ls = sortOn myFst ls
    where
        myFst (a, b, _) = a

rendez1 ls = sortOn (Down . myThird) ls
    where
        myThird (_, b, _) =  b
    

main = do
    print "lab 5 continue"
    mapM_ print $ konyvls
    print "---------sorted---------"
    -- mapM_ print $ rendez konyvls
    mapM_ print $ rendez1 konyvls