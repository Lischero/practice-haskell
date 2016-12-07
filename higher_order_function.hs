multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphaum :: Char -> Bool
isUpperAlphaum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x
{-flip' f x y = f y x -}
    {-where g x y = f y x-}

map' :: (a -> b) -> [a] -> [b]
{-map' _ [] = []
map' f (x:xs) = f x : map' f xs-}
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
{-
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs
-}
filter' p = foldr(\x acc -> if p x then x:acc else acc) []

last' :: [a] -> a
last' = foldl1(\_ x -> x) 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let equalOrSmall = filter (<=x) xs
        larger = filter (>x) xs
    in quicksort equalOrSmall ++ [x] ++ quicksort larger

largestDivisible :: Integer
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3829 == 0 --return Bool--

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n = n : collatz (3 * n + 1)

countCollatz :: Int
countCollatz = length (filter' check (map collatz [1..100]))
    where check xs = length xs >= 15

{- countCollatz = length (filter' (\xs -> length xs > 15) (map collatz [1..100])) -}

sum' :: (Num a) => [a] -> a
{-sum' xs = foldl(\acc x -> acc + x) 0 xs-}
sum' = foldl (+) 0

elem' :: (Eq a) =>  a -> [a] -> Bool
elem' y ys = foldr(\x acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []
--reverse' = foldl (\acc x -> x : acc) []
--reverse' = foldl (flip (:)) [] -- filp(:) :: [a] -> a -> [a]

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSum :: Int
sqrtSum = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

test :: [Int]
test = replicate 2 . product . map (*3) $ zipWith max [1,2][4,5]

fn :: (Floating a, Integral c, RealFrac a) => a -> c
fn = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]


