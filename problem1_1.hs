add :: Int -> Int -> Int
add x y = x + y


modulus :: Int -> Int -> Int
modulus x y = x `mod` y

solveLinearEquation :: Int -> Int -> Int -> Int
solveLinearEquation a b c = b * b - 4 * a * c


elsof :: Double -> Double -> Double
elsof a b
    | b /= 0 = a / b
    | otherwise = error "nullaval osztas"

elsos :: Double -> Double -> Double
elsof_v1 a b = if b/=0 then a/b else error "Nullaval valo osztas"


abs a
    | a < 0 = -a
    | otherwise = a

elojel :: Int -> Int
elojel x
    | x < 0     = -1
    | x == 0    = 0
    | otherwise = 1

main :: IO ()
main = do
    let x = 5
    let y = 3
    let a = -2
    putStrLn ("Összeg: " ++ show (add x y))
    putStrLn ("Elojel " ++ show (elojel a) )
    print (solveLinearEquation 1 4 3)