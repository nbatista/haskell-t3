-- Nome: Nilton Camargo Batista da Silva

-- Questão 1
firstName :: String -> String
firstName [] = []
firstName str
	| head str == ' ' = []
	| otherwise = head str : firstName (tail str)
