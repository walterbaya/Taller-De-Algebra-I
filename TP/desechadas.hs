--Obtiene los subconjuntos de un conjunto de posiciones 


subconjuntosTamanioK :: [[Posicion]] -> Integer -> [[Posicion]]
subconjuntosTamanioK n k | n == [] = [] 
                         |((longitud(head n))==k) = (head n):(subconjuntosTamanioK (tail n) k)               
                         | otherwise = subconjuntosTamanioK (tail n) k 


--Obtiene todas las permutaciones de un conjunto de posiciones (Esta Mal)

--Decide si dado un conjunto que contiene posiciones es un camino
escamino :: [Posicion]->Bool
escamino n | (length n == 1)=True
           | (((fst(head(tail n))) - 1) == fst(head n))&&((((snd(head(tail n))) - 1) == snd(head n))==False) = escamino (tail n)
           | (((snd(head(tail n))) - 1) == snd(head n))&&((((fst(head(tail n))) - 1) == fst(head n))==False) = escamino (tail n)
           | otherwise = False

--Intento de permutaciones
 
 
--Buscar una explicacion con sentido

subconjuntos :: [Posicion] -> [[Posicion]]
subconjuntos []     = [[]]
subconjuntos (x:xs) = (subconjuntos xs) ++ (map (x:) (subconjuntos xs))
   

--Me da todos los subconjuntos formados por las coordenadas de un tablero

conjuntosdeposiciones n = subconjuntos(coordenadas n)


--Me da los subconjuntos de tamaño k formados por las coordenadas de un tablero sin orden
 
conjuntosdetamaniok n k = subconjuntosTamanioK(conjuntosdeposiciones n) k


--Rotar funciona 
rotar :: Integer -> [Posicion] -> [Posicion]
rotar k n | k==1 = n
          | n == [] = [] 
          | otherwise = tail(rotar (k-1) n)++[head(rotar(k-1) n)]

conjuntorotados1 :: Integer -> [Posicion] -> [[Posicion]]
conjuntorotados1 k n |((k==1)==True) = [n]
                     | otherwise = (rotar k n):(conjuntorotados1 (k-1) n)            

conjuntosrotados n = (conjuntorotados1 (longitud n) n)           

--Hasta aca todo bien