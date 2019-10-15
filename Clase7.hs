listar :: a -> a -> a ->[a]
listar a b c = [a,b,c]

sumatoria :: [Integer]->Integer
sumatoria x | x == [] = 0
            | otherwise  = head x + sumatoria (tail x)

pertenece :: Integer -> [Integer]-> Bool
pertenece a x | x == [] = False
              | a == head x = True
              | otherwise = pertenece a (tail x)
              
pertenece1 :: Integer -> [Integer] -> Bool
pertenece1 _ [] = False
pertenece1 a (x:xs) | a==x = True
pertenece1 a (x:xs) = pertenece1 a xs  

--Ejercicio 1
productoria :: [Integer] -> Integer
productoria n | length n > 1 = (head n) * productoria (tail n) 
              | otherwise = head n 

productoriaP :: [Integer] -> Integer
productoriaP (x:xs) | length (x:xs)==1 = x
productoriaP (x:xs) = x* productoria xs    

--Ejercicio 2

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n m | m == [] = []
           | otherwise = ((head m) + n):sumarN n (tail m)

sumarNP :: Integer -> [Integer] -> [Integer]
sumarNP n [] = []           
sumarNP n (x:xs) = (x+n): sumarNP n xs

--Ejercicio 3 

elultimo :: [Integer] -> Integer
elultimo (x:[])= x
elultimo (x:xs)= elultimo xs

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo n | tail n == [] = [2*head n]
                | otherwise = ((head n) + (elultimo n)):sumarElUltimo (tail n)

sumarElUltimoP :: [Integer] -> [Integer]
sumarElUltimoP (x:[])= [2*x]
sumarElUltimoP (x:xs)= (x + elultimo (x:xs)):(sumarElUltimoP xs)                                 

--Ejercicio 4

invertir :: [Integer] -> [Integer]
invertir n | tail n == [] = n
           | otherwise = invertir (tail n) ++ [head n]

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero n = invertir (sumarElUltimo(invertir n))
