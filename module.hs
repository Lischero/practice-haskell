import Data.List
import Data.Char

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
