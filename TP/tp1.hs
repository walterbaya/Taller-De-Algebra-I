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



--Funciones implementadas

--Ejercicio 1

--Funcion auxiliar que calcula el maximo de una fila desde la posocion de columna 1 hasta j.

maxFilaHastaJ :: Tablero -> Posicion -> Integer
maxFilaHastaJ s (i,j) |j == 1 = valor s (i,1)
                      |((valor s (i,j) > maxFilaHastaJ s (i,(j-1))) == True) = valor s (i,j)
                      |otherwise = maxFilaHastaJ s (i,(j-1))

--Funcion auxiliar que calcula el maximo de una fila i

maxFila :: Tablero -> Integer -> Integer
maxFila s i = maxFilaHastaJ s (i,cantidadColumnas s)

-- Funcion auxiliar que calcula el maximo valor desde la fila 1 hasta la fila i.

maximoHastaFila :: Tablero -> Integer -> Integer 
maximoHastaFila s 1 = maxFila s 1
maximoHastaFila s i | maxFila s i > maximoHastaFila s (i-1) = maxFila s i
maximoHastaFila s i = maximoHastaFila s (i-1)


--FUNCION MAXIMO

maximo :: Tablero -> Integer
maximo s = maximoHastaFila s x
           where x = cantidadFilas s


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

--Funciones Auxiliar que calcula

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

--FUNCION NUMEROS DE CAMINOS

numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino n m | (((length m ) == 1)==True) = [valor n (head m)]
                    | otherwise = (valor n (head m)):(numerosDeCamino n (tail m)) 


--Ejercicio 5

--Caminos de Fibonacci 

caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci n m | (length m == 1)|| (length m == 2) = True
                      | ((valor n (head(tail(tail m))) == (valor n (head(tail m))) + valor n (head m)) == True) = caminoDeFibonacci n (tail m)
                      | otherwise = False 


--PARA EJERCICIO 6
--FUNCION NOVACIO

novacio :: Tablero -> Integer -> Conjunto[Integer]
novacio n k | secuencias n k == [] = novacio n (k-1)
            | otherwise = head(secuencias n k)

mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci n = novacio n ((cantidadFilas n)*(cantidadColumnas n))












