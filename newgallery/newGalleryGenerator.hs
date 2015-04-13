--Nome: Nilton amargo Batista da Silva
-- Este programa ilustra:
-- 1) Uso de registros (record syntax) em Haskell
-- 2) Leitura de arquivo CSV
--

--
-- Declara novo tipo de dado 'GalleryItem' usando record syntax
-- Cada GalleryItem é um registro formado por 3 campos / atributos
-- Com esta sintaxe, Haskell automaticamente cria funções de acesso aos campos
-- Por exemplo, dado um GalleryItem x, 'title x' vai retornar o atributo 'title' de x,
-- 'description x' vai retornar o atributo 'description' de x, e assim por diante
-- Mais sobre isso em: http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--

import Text.Printf
import Data.Char
import Data.String
import Data.List


data GalleryItem =
   GalleryItem {title :: String,
                description :: String,
                author :: String}
               
-- Converte uma lista de String em um GalleryItem
toGalleryItem :: [String] -> GalleryItem
toGalleryItem [s1, s2, s3] = GalleryItem {title = s1, description = s2, author = s3}


-- Funcao principal que faz leitura de arquivo e mostra atributos de um item da galeria
main :: IO ()
main = do
    strcontent <- readFile infile				-- lê conteúdo do arquivo em string
    let strlist = map (splitOnChar ';') (lines strcontent)	-- extrai linhas e quebra cada uma delas
        itemlist = map (toGalleryItem) strlist			-- transforma cada linha num GalleryItem
        item = head itemlist 					-- escolhe um item da lista
    writeFile outfile (htmlGen itemlist)
   -- putStrLn (title item)					-- mostra titulo do item
   -- putStrLn (author item)				-- mostra autor do item
    putStrLn "Feito!"
    where 
    outfile = "gallery.html"
    infile = "data.csv"


htmlGen :: [GalleryItem] -> String
htmlGen itemlist = gerarTitulo ++ (intercalaStr [imageDiv(y) | y <- pictures] [separarItem (x) | x <- itemlist]) ++ "</body>\n</html>"



intercalaStr :: [String] -> [String] -> String
intercalaStr [] [] = []
intercalaStr str1 str2 = (head str1 ++ "</div><p>" ++ head str2) ++ (intercalaStr (tail str1) (tail str2))



separarItem :: GalleryItem -> String
separarItem item = "<b>" ++ title(item) ++ "</b>" ++ "<br/>\n" ++ description(item) ++ "<br/>\n" ++ author(item) ++ "<br/>\n\n"


-- Conteudo da galeria
pictures :: [(String, String)]
pictures =
	[("T1_1.png","Imagem 1"),
	 ("T2_1.png","Imagem 2"),
	 ("T3_1.png","Imagem 3")]


-- Gera a imagem a partir dos itens da função pictures
gerarImagem :: [(String,String)] -> String
gerarImagem items = concat [imageDiv x | x <- items]


-- Gera uma string que mostra a imagem e seu título
imageDiv :: (String, String) -> String
imageDiv (imgfile, title) = "<div class=csslisting><div class=floatbox><a href=image.html?src=./images/" ++ imgfile ++ "&height=450&alt=" ++ title ++ "><img src=./images/" ++ imgfile ++ " width=200  border=0 /></a>\n\n"

-- Funcão que gera título e cabeçalho em HTML
gerarTitulo :: String
gerarTitulo = " <html>\n <head>\n <title>Galeria de Trabalhos de Paradigmas de Programação</title>\n <link href=style.css rel=stylesheet type=text/css />\n </head>\n <body>\n <h1>Galeria de Trabalhos de Paradigmas de Programação</h1>\n\n\n"





-- Funcao que decompoe string usando um caracter delimitador
splitOnChar :: Char -> String -> [String]
splitOnChar x y = auxSplitOnChar x y [[]]

auxSplitOnChar :: Char -> String -> [String] -> [String]
auxSplitOnChar x [] z = reverse (map reverse z)
auxSplitOnChar x (y:ys) (z:zs) = 
	if y == x then 
            auxSplitOnChar x ys ([]:(z:zs)) 
        else 
            auxSplitOnChar x ys ((y:z):zs)
            
