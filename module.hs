import Data.List
import Data.Char
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
--wordNums s = map (\xs -> (head xs, length xs)) (group (sort (words s)))
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words

isEn :: (Eq a) => [a] -> [a] -> Bool
needle `isEn` heystack = any (needle `isPrefixOf`) $ tails heystack

encode :: Int -> String -> String
encode offset msg = map (\x -> chr $ ord x + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
--decode shift msg = map (chr . (+ negate shift) . ord) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n)[1..]

{--
findkey :: (Eq k) => k -> [(k,v)] -> v
findkey key  = snd . head . filter (\(x,y) -> key == x)
--}

--fix error Versions
--recursion Ver
{--
findkey :: (Eq k) -> k -> [(k,v)] -> Maybe v
findkey key [] = Nothing
findkey key [(x,y):xs]
    | key == x = Just y
    | otherwise findkey key xs
--}

--foldr Ver
findkey :: (Eq k) => k -> [(k,v)] -> Maybe v
findkey key xs = foldr (\(x,y) acc -> if key == x then Just y else acc ) Nothing xs

--数字文字列をリストへ変換するやつ。
string2digit :: String -> [Int]
string2digit =  map digitToInt . filter isDigit


