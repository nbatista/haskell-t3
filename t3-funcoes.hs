-- Nome: Nilton Camargo Batista da Silva

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
