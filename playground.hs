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

abs' :: Num a => [a] -> a
abs' x
  | x >= 0    = x
  | otherwise = (-x)

myFunc :: Integer -> Integer -> [Integer]
myFunc start end = map (\x -> abs' x) [start..end]
