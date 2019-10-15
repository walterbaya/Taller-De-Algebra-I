-- Paola Diaz Trujillo 301/16
-- Walter Ariel Baya 368/18
-- Ader Landeo Sacha 298/18



type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33,53, 16], [27, 2, 8, 18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)], [0, 10, 20], [(-10), (-10), 0], [10, 20,(-10)]]

sopa3 :: Tablero
sopa3 = [[10,5,15],[-1,7,2],[2,12,3]]

camino1 :: Camino 
camino1 = [(1,1),(1,2),(2,2),(2,3)]

camino2 :: Camino 
camino2 = [(2,1),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino 
camino3 = [(1,2),(2,2),(3,2)]

-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t)



--Ejercicio 1:

--Funcion auxiliar que calcula el maximo de una lista

maxlista :: [Integer] -> Integer
maxlista [x] = x
maxlista (x:y:ys) = max x (maxlista (y:ys))

--FUNCION MAXIMO (devuelve el maximo valor del tablero)

maximo :: [[Integer]] -> Integer 
maximo [t] = maxlista t 
maximo (t:ts) = max (maxlista t) (maximo (ts))


--Ejercicio 2

--Funcion auxiliar que calcula las apariciones del valor del numero con coordenadas (a,b) sobre una fila i

aparicionesSobreFila :: Tablero -> Posicion -> Posicion -> Integer
aparicionesSobreFila s (a,b) (i,j) | j == 0 =  0
                                   | (valor s (a,b)==valor s (i,j))==True = 1 + aparicionesSobreFila s (a,b) (i,(j-1))
                                   | otherwise = aparicionesSobreFila s (a,b) (i,(j-1))

aparicionesSobreFila1 :: Tablero -> Posicion -> Integer -> Integer
aparicionesSobreFila1 s (a,b) i = aparicionesSobreFila s (a,b)(i,x)
                                  where x = cantidadColumnas s

--Funcion auxiliar que calcula las apariciones totales del valor del numero con coordenadas (a,b)

aparicionesHastaFila ::Tablero -> Posicion -> Integer -> Integer
aparicionesHastaFila s (a,b) 0 = 0
aparicionesHastaFila s (a,b) i = x + aparicionesHastaFila s (a,b) (i-1)
                                where x = aparicionesSobreFila1 s (a,b) i

apariciones :: Tablero -> Posicion -> Integer                               
apariciones s (a,b) = aparicionesHastaFila s (a,b) (cantidadFilas s)
                      where x = (cantidadFilas s)   

--Funcion auxiliar que  calcula las apariciones de los valores de la fila a sobre la sopa

aparicionesfila :: Tablero -> Posicion -> [(Integer,Integer)]
aparicionesfila s (a,0) = []
aparicionesfila s (a,b) = x : aparicionesfila s (a,b-1)
                          where x = (valor s (a,b),apariciones s (a,b)) 

--Funcion Auxiliar que calcula las apariciones de los valores sobre una fila a desde la columna 1 a la de mayor valor posible

aparicionesfila1 :: Tablero -> Integer -> [(Integer,Integer)]
aparicionesfila1 s a = aparicionesfila s (a,x) 
                      where x = (cantidadColumnas s)

aparicionestotales :: Tablero -> Integer -> [(Integer,Integer)]
aparicionestotales s 0 = []
aparicionestotales s a = x++(aparicionestotales s (a-1))
                        where x = (aparicionesfila1 s a)
                        
aparicionestotales1 :: Tablero -> [(Integer,Integer)]
aparicionestotales1 s = aparicionestotales s x 
                        where x = cantidadFilas s  

valormayorsegunda :: [(Integer,Integer)] -> Integer
valormayorsegunda x |((length x == 1) == True) = snd(head x)
                    |(snd(head x) >= valormayorsegunda(tail x)) == True = snd(head x)
                    |otherwise = valormayorsegunda(tail x)

valorsegundo :: Integer -> [(Integer,Integer)] -> Integer
valorsegundo a x |(x == []) = 0 
                 |((snd(head x) == a)==True) = fst(head x)
                 |otherwise = valorsegundo a (tail x)

--FUNCION MAS REPETIDO

masRepetido :: Tablero -> Integer
masRepetido s = valorsegundo(valormayorsegunda(aparicionestotales1 s)) (aparicionestotales1 s)

--Ejercicio 3

--Halla los numeros de los casilleros de un camino

numcamino :: [[Integer]] -> [(Integer,Integer)] -> [Integer]
numcamino t [] = [] 
numcamino t (x:xs) = valor t x : numcamino t (xs)

--Ejercicio 4

--Se fija si hay repetidos en una lista

hayrepetidos :: [Integer] -> Bool
hayrepetidos [x] = False
hayrepetidos (x:y:ys) | x == y = True
                      | otherwise = hayrepetidos (x:ys) || hayrepetidos (y:ys)

--CAMINO SIN REPETIDOS

caminoSinRepetidos :: [[Integer]] -> [(Integer,Integer)] -> Bool
caminoSinRepetidos t (x:xs) = not ( hayrepetidos ( numcamino t (x:xs) ))

--Ejercicio 5

--Se fija si la suma de los dos primeros da el tercero en un camino

aux :: [Integer] -> Bool
aux [x] = True
aux [x,y] = True
aux (x:y:w:ws) | (x + y == w) && aux (y:w:ws) = True
                 | otherwise = False

--CAMINO DE FIBONACCI

caminodefibonacci :: [[Integer]] -> [(Integer ,Integer )] -> Bool
caminodefibonacci t (p:ps) = aux ( numcamino t (p:ps) )

--Ejercicio 6

--Devuelve una lista de las de mayor tamaño 

novacio :: Tablero -> Integer -> [Integer]
novacio n k | secuenciasdefibonaccidelongitudk n k == [] = novacio n (k-1)
            | otherwise = head(secuenciasdefibonaccidelongitudk n k)

--MAYORSECUENCIADEFIBONACCI
mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci n = novacio n ((cantidadFilas n)+(cantidadColumnas n)) 

--Ejercicio 7

--Auxiliar de la funcion coordenadas

sol :: [Integer] -> Integer -> Camino
sol [] _ = [ ]
sol (x:xs) n = (n,x) : sol xs n

--Funcion que calcula las coordenadas de un tablero
coordenadas :: Tablero -> Integer -> Camino
coordenadas [] 0 = []
coordenadas (t:ts) n = coordenadas ts (n-1) ++ sol [1..m] n
             where n = cantidadFilas (t:ts)
                   m = cantidadColumnas (t:ts)


agregar :: Camino -> [Camino] -> [Camino]
agregar x [] = []
agregar x (p:ps) =  (x ++ p) : agregar x ps

partes :: Camino -> [Camino]
partes [] = [[]]
partes (p:ps) = partes (ps) ++ (agregar [p] (partes ps))


compara :: Posicion -> Posicion -> Bool
compara (i,j) (k,l) | (i==k && j+1==l) || (i+1==k && j==l) = True
                              | otherwise = False

escamino :: Camino -> Bool
escamino [] = False
escamino [x] = True
escamino [x,y] = compara x y 
escamino (x:y:w:ws) | (((compara  x y ) == True ) && ((compara y w) == True)) = escamino (w:ws)
                    | otherwise = False
 
siescamino :: [Camino] -> [Camino]
siescamino [] = []
siescamino (p:ps) | (escamino p == True) = p : siescamino ps
                  | otherwise = siescamino (ps)


listadecamino:: Tablero -> [Camino] -> [[Integer]]
listadecamino t [] = []
listadecamino t (p:ps) = numcamino t p : listadecamino t (ps)


fibo:: Tablero -> Tablero
fibo [] = []
fibo (x:xs) | (aux x == True) = x : fibo xs
            | otherwise = fibo xs

repetidos :: Tablero -> Tablero
repetidos [x] = [x]
repetidos (t:ts) | elem t ts = repetidos ts
                 | otherwise = t : repetidos ts 

secuencias :: Tablero -> Integer -> Tablero
secuencias [] k = [] 
secuencias (t:ts) k |fromIntegral (length t) == k = t : secuencias ts k
                    | otherwise = secuencias ts k

secuenciasdefibonaccidelongitudk :: Tablero -> Integer -> Conjunto[Integer]
secuenciasdefibonaccidelongitudk (t:ts) k = secuencias ( repetidos ( fibo ( listadecamino (t:ts) ( siescamino ( partes ( coordenadas (t:ts) k)))))) k

  



