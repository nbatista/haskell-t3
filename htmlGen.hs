--Nome: Nilton Camargo Batista da Silva
-- Este programa lê um arquivo CSV, com 2 dados por linha. Cada linha é transformada numa tupla. 
-- A lista de tuplas é passada para uma função que vai gerar uma longa string representando um
-- documento HTML.
--

main :: IO ()
main = do
    strcontent <- readFile infile
    let listofstrlist = map (splitOnChar ',') (lines strcontent)
        strtuplelist = map (\lis -> (head lis, last lis)) listofstrlist
    writeFile outfile (mkHtmlURLItemsDoc "Usuários Cadastrados no NCC" strtuplelist)
    putStrLn "Feito!"
    where 
    infile = "logins.csv"
    outfile = "output.html"
	
	


-- Esta função deve ser alterada para chamar outras funções que vão
-- construir o documento HTML
mkHtmlURLItemsDoc :: String -> [(String,String)] -> String
mkHtmlURLItemsDoc title lis = titleGen (title) ++ htmlGen (lis)


titleGen :: String -> String
titleGen [] = []
titleGen title = "</html>\n\n<head>\n<title>"++title++"</title>\n</head>\n"


htmlGen :: [(String,String)] -> String
htmlGen tuple = "\n<body>\n<ul>\n" ++ (concat [linkGen(x) | x <- tuple]) ++ "</ul>\n</body>\n\n</html>"


linkGen :: (String,String) -> String
linkGen tuple = "<li><a href="++"http://www.inf.ufsm.br/~" ++ username ++ ">" ++ nome ++ "</a></li>\n"
	where nome = fst (tuple)
	      username = snd (tuple)


-- Decompoe string usando um caracter delimitador
splitOnChar :: Char -> String -> [String]
splitOnChar x y = auxSplitOnChar x y [[]]

auxSplitOnChar :: Char -> String -> [String] -> [String]
auxSplitOnChar x [] z = reverse (map reverse z)
auxSplitOnChar x (y:ys) (z:zs) = 
	if y == x then 
            auxSplitOnChar x ys ([]:(z:zs)) 
        else 
            auxSplitOnChar x ys ((y:z):zs)