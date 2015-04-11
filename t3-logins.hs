import Data.Char
import Data.String
import Data.List

-- Para solucionar o problema da lesta de logins usei a quest�o 4 
-- do trabalho qfun��es t3 que pegar� o seu primeiro nome
userName :: String -> String
userName [] = []
userName str = toLower (head str) : map toLower (lastName str)

--Tamb�m usei a quest�o 3 para pegar seu ultimo sobrenome

lastName :: String -> String
lastName [] = []
lastName str = reverse (lastNameAux str)

lastNameAux :: String -> String
lastNameAux [] = []
lastNameAux str
	| last str == ' ' = []
	| otherwise = last str : lastNameAux (init str)


-- Lista de logins  
main :: IO ()
main = do
    strcontent <- readFile "nomes.csv"
    let strlist = lines strcontent
        strnew = [x ++ "," ++ userName x | x <- strlist]
    writeFile "logins.csv" (unlines strnew)
	putStrLn "Feito!"
	outfile = "login.html"
