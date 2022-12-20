import qualified Data.Map as Map
import Data.List
import Data.Ord

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


class Size a
  where size :: a -> Int

instance Size Int
  where size x = abs x

instance Size [a]
  where size xs = length xs

class BasicEq a where
  isEqual :: a -> a -> Bool

instance BasicEq Bool where
  isEqual True True   = True
  isEqual False False = True
  isEqual _     _     = False

class BasicEq2 a where
  isEqual2    :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool

class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  -- isEqual3 x y = not (isNotEqual3 x y)

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)

-- class Eq' a where
--   (==), (/=) :: a -> a -> Bool

--     -- Minimal complete definition
--     --      (==) or (/=)
--   x /= y    =  not (x == y)
--   x == y    =  not (x /= y)

data Color = Red | Green | Blue
  deriving Show

instance BasicEq3 Color where
  isEqual3  Red Red     = True
  isEqual3  Green Green = True
  isEqual3  Blue Blue   = True
  isEqual3  _    _      = False


-- instance Show Color where
--   show Red    =  "Dark Red"

go' = do
  putStrLn "Please enter a Double"
  inpStr <- getLine
  let inpDouble = (read inpStr)::Double
  putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

main = do
        putStrLn "Please enter a Double:"
        inpStr <- getLine
        let inpDouble = (read inpStr)::Double
        putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

type Car = String
type Point = (Double, Double)

myPos:: Double -> Point
myPos x = (x, sin x)

safeHead :: [a] -> Either String a
safeHead [] = Left "I have no head."
safeHead (x:_) = Right x

-- data Person = Person String String String
--   deriving Show

-- kevin = Person "kevin" "st. charles" "555-5555"

-- getName :: Person -> String
-- getName (Person name _ _) = name

-- data Person' = Person' {name::String, location::String, number::String}
--   deriving Show

-- bruce = Person' "bruce" "seattle" "235-3444"

-- getName' :: Person' -> name
-- getName' name = name 

data Vector a = Vector a a a
  deriving Show

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

vectMult :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector x1 y1 z1) mult = Vector (x1*mult) (y1*mult) (z1*mult)

data Money = Money Int
  deriving Show

renderMoney :: Money -> String
renderMoney (Money cents) = show (fromIntegral cents / 100)

(+!) :: Money -> Money -> Money
(Money a) +! (Money b) = Money (a+b)

scale :: Money -> Double -> Money
scale (Money a) x = Money (round (fromIntegral a * x))

addVat :: Money -> Money
addVat m = m +! scale m 0.24

-- Declared also above ln154
data Person = Person {name :: String, age :: Int}
  deriving Show

data SortOrder = Ascending | Descending
data SortField = Name | Age

sortByField :: SortField -> [Person] -> [Person]
sortByField Name ps = sortBy (comparing name) ps
sortByField Age ps = sortBy (comparing age) ps

sortPersons :: SortField -> SortOrder -> [Person] -> [Person]
sortPersons field Ascending ps = sortByField field ps
sortPersons field Descending ps = reverse (sortByField field ps)

persons = [Person "Fridolf" 73, Person "Greta" 60, Person "John" 58]
