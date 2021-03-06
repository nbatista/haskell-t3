-- Nome: Nilton Camargo Batista da Silva
import Data.Char
import Data.String
import Data.List
import System.IO


-- Quest�o 1

firstName :: String -> String
firstName [] = []
firstName str
	| head str == ' ' = []
	| otherwise = head str : firstName (tail str)


-- Quest�o 2

firstName' :: String -> String
firstName' str = takeWhile (/= ' ') str

-- Quest�o 3

lastName :: String -> String
lastName [] = []
lastName str = reverse (lastNameAux str)

lastNameAux :: String -> String
lastNameAux [] = []
lastNameAux str
	| last str == ' ' = []
	| otherwise = last str : lastNameAux (init str)


-- Quest�o 4

userName :: String -> String
userName [] = []
userName str = toLower (head str) : map toLower (lastName str)

-- Questao 5

encodeName :: String -> String
encodeName str = concat [substituir (x) | x <- listStr(str)]

listStr :: String -> [String]
listStr [] = []
listStr str = [ [x] | x <- str]

substituir :: String -> String
substituir str
	| str == "a" = "4"
	| str == "A" = "4"
	| str == "e" = "3"
	| str == "E" = "3"
	| str == "i" = "1"
	| str == "I" = "1"
	| str == "o" = "0"
	| str == "O" = "0"
	| str == "u" = "00"
	| str == "U" = "00"
	| otherwise = str


-- Quest�o 6

isElem :: Int -> [Int] -> Bool
isElem _ [] = False
isElem x lis = any (== x) lis


-- Quest�o 7

contVogais :: String -> Int
contVogais [] = 0
contVogais str 
 | (checaVogal (head str)) == True = 1 + contVogais (tail str)
 | otherwise = contVogais (tail str)

checaVogal :: Char -> Bool
checaVogal c = if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') then True else False


-- Quest�o 8

contCons :: String -> Int
contCons [] = 0
contCons str = length (filter (checaCons) str)

checaCons :: Char -> Bool
checaCons c = if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') then False else True


-- Quest�o 9

isInt :: String -> Bool
isInt [] = False
isInt str = if (length (filter (isLetter) str) > 0) then False else True


-- Quest�o 10

strToInt :: String -> Int
strToInt str = read str :: Int