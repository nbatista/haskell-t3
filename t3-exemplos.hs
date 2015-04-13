--Nome: Nilton Camargo Batista da Silva

import Data.Char
import Data.String
import Data.List
--As fun��es de alta ordem any e all s�o
--pr�-definidas na biblioteca Prelude do Haskell (veja se��o Special Folds).
--Estude e teste essas fun��es e apresente 2 exemplos de uso de cada uma delas.

-- any e all verificam se um predicado � verdadeiro para no m�nimo um ou todos os elementos da lista (respectivamente). 
-- referencias:
-- http://zvon.org/other/haskell/Outputprelude/any_f.html
-- http://zvon.org/other/haskell/Outputprelude/
-- http://haskell.tailorfontela.com.br/modules

-- Exemplo ANY

algum :: Int -> [Int] -> Bool
algum num lista = any (==num) lista

algumL :: [String] -> [String] -> Bool
algumL str str1 = any (`elem` str)str1


-- Exemplo ALL
todos :: Int -> [Int] -> Bool
todos num lista = all (==num) lista

todosL :: [String] -> [String] -> Bool
todosL str str1 = all (`elem` str)str1



--Fun��o de alta ordem $
--A fun��o ($) � chamado de aplica��o de fun��o.
--Faz fun��es associativa � direita
-- referencias:
--http://haskell.tailorfontela.com.br/higher-order-functions
--http://shuklan.com/haskell/lec06.html#/0/13

--Exemplo $

asso :: [Int] -> Int
asso num =  sum $ filter (> 10) $ map (*2) num

func :: [Int] -> Bool
func lista = and $ map (>4) lista

--Fun��o compostas em haskell

-- referencias:
--http://shuklan.com/haskell/lec06.html#/0/14
--http://haskell.tailorfontela.com.br/higher-order-functions
--https://arsphysica.wordpress.com/2011/04/18/haskell1/

comp :: [Int] -> [Int]
comp num = map (negate . abs) num

comp1 :: [[Int]] -> [Int]
comp1 lista = map (negate . sum . tail)lista