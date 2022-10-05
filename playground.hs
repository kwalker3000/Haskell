import qualified Data.Map as Map
import Data.List

readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsupported string: " ++ s)

iWantAString :: Either Int String -> String
iWantAString (Right str)   = str
iWantAString (Left number) = show number

weekDays :: [Either String Int]
weekDays = [Left "Sunday", Right 1]

describe :: Integer -> String
describe 0 = "zero"
describe 1 = "one"
describe 2 = "an even prime"
describe n = "the number " ++ show n

describe' :: Integer -> String
describe' n = case n of 0 -> "zero"
                        1 -> "one"
                        2 -> "an even prime"
                        n -> "the number " ++ show n

sentenceType :: String -> String
sentenceType sentence = case last sentence of '.' -> "statement"
                                              '?' -> "question"
                                              '!' -> "exclamation"
                                              _   -> "not a sentence"

justBoth a b = [Just a, Just b]

applyTo1 :: (Int -> Int) -> Int
applyTo1 f = f 1

addThree :: Int -> Int
addThree x = x + 3

countAWords :: String -> Int
countAWords string = length (filter startsWithA (words string))
  where startsWithA s = head s == 'a'

abs' :: Int -> Int
abs' x
  | x >= 0    = x
  | otherwise = (-x)


cartesianProduct :: [a] -> [a] -> [(a,a)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

(*!) :: [a] -> [a] -> [(a, a)]
xs *! ys = cartesianProduct xs ys

mytest :: String -> String
--mytest [] = False
mytest (x:xs) 
 | x == '(' || x == '[' || x == '{'  = x : mytest xs ++ []
 | otherwise                         = x : []


-- | x == ')' || x == ']' || x == '}'  = x : []

isBalanced [] = "empty"
isBalanced [_] = "one element"
--isBalanced xs = isBalanced $ (init . init) (mytest xs)
isBalanced xs = (mytest xs)

getOps :: String -> String
getOps xs = filter isOps xs
  
isOps :: Char -> Bool
isOps x = case x of
  '(' -> True
  ')' -> True
  '[' -> True
  ']' -> True
  '{' -> True
  '}' -> True
  _   -> False

isPair :: String -> Bool
isPair xs
  | xs == "()" || xs == "[]" || xs == "{}" = True
  | otherwise                              = False



map' g xs = foldr helper [] xs
  where helper y ys = g y : ys


--Map.fromList :: Ord k => [(k, a)] -> Map.Map k a

--val = Map.fromList [("a", 1), ("b", 2)]

val = Map.fromList (zip "abcdefghijklmnopqrstuvwxyz" [1..])

geometric k1 _ 1 = k1 
geometric k1 r k = k1 + geometric (k1*r) r (k-1) 


geometric' k1 r k = (k1 * (1-r^k))/(1-r)

geometricTerm k1 _ 1 = k1
geometricTerm k1 r k = geometricTerm (k1*r) r (k-1)
