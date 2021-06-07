-- Prática 04 de Haskell
-- Nome: Juliano de Mello Pasa

-- Exercicio 1
-- Classificacao de IMC
bmi :: Float -> Float -> String
bmi w h =
  let expr = w / (h^2)
  in if expr >= 30.0 then "ACIMA" else if expr <= 18.5 then "ABAIXO" else "NORMAL"

-- Exercicio 2
-- Classificacao de IMC usando where
bmi' :: Float -> Float -> String
bmi' w h 
  | expr >= 30.0 = "ACIMA"
  | expr > 18.5 = "NORMAL"
  | otherwise = "ABAIXO"
  where expr = w / (h^2) 

-- Exercicio 3
-- Alteracao nas funcoes de validacao de cpf
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11-expr

-- Exercicio 4
-- Tabela de AND logico
andTable :: [(Bool, Bool, Bool)]
andTable = [(x, y, x && y) | x <- [True, False], y <- [True, False]]