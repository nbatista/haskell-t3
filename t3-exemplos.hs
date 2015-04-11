--Nome: Nilton Camargo Batista da Silva

import Data.Char
import Data.String
import Data.List
--As funções de alta ordem any e all são
--pré-definidas na biblioteca Prelude do Haskell (veja seção Special Folds).
--Estude e teste essas funções e apresente 2 exemplos de uso de cada uma delas.

-- any e all verificam se um predicado é verdadeiro para no mínimo um ou todos os elementos da lista (respectivamente). 
-- referencias:
-- http://zvon.org/other/haskell/Outputprelude/any_f.html
-- http://zvon.org/other/haskell/Outputprelude/
-- http://haskell.tailorfontela.com.br/modules

-- Exemplo ANY

algum :: Int -> [Int] -> Bool
algum num lista = any (==num) lista


-- Exemplo ALL
todos :: Int -> [Int] -> Bool
todos num lista = all (==num) lista


--Função de alta ordem $
--A função ($) é chamado de aplicação de função.
--Faz funções associativa à direita
-- referencias:
--http://haskell.tailorfontela.com.br/higher-order-functions
--http://shuklan.com/haskell/lec06.html#/0/13

--Exemplo $

asso :: [Int] -> Int
asso num =  sum $ filter (> 10) $ map (*2) num


--Função compostas em haskell

-- referencias:
--http://shuklan.com/haskell/lec06.html#/0/14
--http://haskell.tailorfontela.com.br/higher-order-functions
--https://arsphysica.wordpress.com/2011/04/18/haskell1/
comp :: [Int] -> [Int]
comp num = map (negate . abs) num