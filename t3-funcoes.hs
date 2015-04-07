-- Nome: Nilton Camargo Batista da Silva
import Data.Char
import Data.String
import Data.List
import System.IO


-- Questão 1
firstName :: String -> String
firstName [] = []
firstName str
	| head str == ' ' = []
	| otherwise = head str : firstName (tail str)


-- Questão 2
firstName' :: String -> String
firstName' str = takeWhile (/= ' ') str

-- Questão 3
lastName :: String -> String
lastName [] = []
lastName str = reverse (lastNameAux str)

lastNameAux :: String -> String
lastNameAux [] = []
lastNameAux str
	| last str == ' ' = []
	| otherwise = last str : lastNameAux (init str)


-- Questão 4

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