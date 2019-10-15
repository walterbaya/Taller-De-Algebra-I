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

sopa4 :: Tablero
sopa4 = [[1,2],[3,4]]


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

--Funcion auxiliar que calcula el maximo de una fila hasta j.

maxFilaHastaJ :: Tablero -> Posicion -> Integer
maxFilaHastaJ s (i,j) |j == 1 = valor s (i,1)
                      |((valor s (i,j) > maxFilaHastaJ s (i,(j-1))) == True) = valor s (i,j)
                      |otherwise = maxFilaHastaJ s (i,(j-1))

--Funcion auxiliar que calcula el maximo de una fila i

maxFila :: Tablero -> Integer -> Integer
maxFila s i = maxFilaHastaJ s (i,cantidadColumnas s)

-- Funcion auxiliar que calcula el maximo valor desde la fila 1 hasta  la fila i.

maximoHastaFila :: Tablero -> Integer -> Integer 
maximoHastaFila s i | ((i == 1) == True) = maxFila s 1
                    | maxFila s i > maximoHastaFila s (i-1) = maxFila s i
                    | otherwise = maximoHastaFila s (i-1)

--FUNCION MAXIMO

maximo :: Tablero -> Integer
maximo s = maximoHastaFila s (cantidadFilas s)


--Ejercicio 2

--Funcion auxiliar que calcula las apariciones del valor del numero con coordenadas (a,b) sobre una fila i

aparicionesSobreFila :: Tablero -> Posicion -> Posicion -> Integer
aparicionesSobreFila s (a,b) (i,j) | j == 0 =  0
                                   | (valor s (a,b)==valor s (i,j))==True = 1 + aparicionesSobreFila s (a,b) (i,(j-1))
                                   | otherwise = aparicionesSobreFila s (a,b) (i,(j-1))
aparicionesSobreFila1 :: Tablero -> Posicion -> Integer -> Integer
aparicionesSobreFila1 s (a,b) i = aparicionesSobreFila s (a,b)(i,cantidadColumnas s)
--Funcion auxiliar que calcula las apariciones totales del valor del numero con coordenadas (a,b)

aparicionesHastaFila ::Tablero -> Posicion -> Integer -> Integer
aparicionesHastaFila s (a,b) i | ((i==0)==True) = 0
                               | otherwise = aparicionesSobreFila1 s (a,b) i + aparicionesHastaFila s (a,b) (i-1)

apariciones :: Tablero -> Posicion -> Integer                               
apariciones s (a,b) = aparicionesHastaFila s (a,b) (cantidadFilas s)   

--Funcion auxiliar que  calcula las apariciones de los valores de la fila a sobre la sopa

aparicionesfila :: Tablero -> Posicion -> [(Integer,Integer)]
aparicionesfila s (a,b) | b == 0 = []
                        | otherwise = (valor s (a,b),apariciones s (a,b)) : aparicionesfila s (a,b-1)

aparicionesfila1 :: Tablero -> Integer -> [(Integer,Integer)]
aparicionesfila1 s a = aparicionesfila s (a,cantidadColumnas s)   

aparicionestotales :: Tablero -> Integer -> [(Integer,Integer)]
aparicionestotales s a |((a == 0)==True) = []
                       | otherwise = (aparicionesfila1 s a)++(aparicionestotales s (a-1))
aparicionestotales1 :: Tablero -> [(Integer,Integer)]
aparicionestotales1 s = aparicionestotales s (cantidadFilas s)   

--FUNCION MASREPETIDOS
valormayorsegunda :: [(Integer,Integer)] -> Integer
valormayorsegunda x |((length x == 1) == True) = snd(head x)
                    |(snd(head x) >= valormayorsegunda(tail x)) == True = snd(head x)
                    |otherwise = valormayorsegunda(tail x)

valorsegundo :: Integer -> [(Integer,Integer)] -> Integer
valorsegundo a x |(x == []) = 0 
                 |((snd(head x) == a)==True) = fst(head x)
                 |otherwise = valorsegundo a (tail x)

masRepetido :: Tablero -> Integer
masRepetido s = valorsegundo(valormayorsegunda(aparicionestotales1 s)) (aparicionestotales1 s)
																
numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino n m | (((length m ) == 1)==True) = [valor n (head m)]
                    | otherwise = (valor n (head m)):(numerosDeCamino n (tail m)) 

--Caminos de Fibonacci 

caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci n m | (length m == 1)|| (length m == 2) = True
                      | ((valor n (head(tail(tail m))) == (valor n (head(tail m))) + valor n (head m)) == True) = caminoDeFibonacci n (tail m)
                      | otherwise = False    





--Funciones Auxiliares

longitud :: [Posicion] -> Integer
longitud [] = 0
longitud n = 1 + longitud(tail n)

--Funciones De Coordenadas

coordenadassobrefila1 :: Tablero -> Integer -> Integer -> [Posicion]
coordenadassobrefila1 n k 1 = [(k,1)]
coordenadassobrefila1 n k c = (k,c):(coordenadassobrefila1 n k (c-1))   

coordenadassobrefila :: Tablero -> Integer -> [Posicion]
coordenadassobrefila n k = coordenadassobrefila1 n k (cantidadColumnas n)

coordenadashastacolumnak :: Tablero -> Integer -> [Posicion]
coordenadashastacolumnak n 1 = (coordenadassobrefila n 1)
coordenadashastacolumnak n k = (coordenadassobrefila n k)++(coordenadashastacolumnak n (k-1))

coordenadas :: Tablero -> [Posicion]
coordenadas n = coordenadashastacolumnak n (cantidadFilas n)

individuales :: [Posicion] -> [[Posicion]] 
individuales [] = [] 
individuales (x:xs) = [x]:(individuales(xs))

--FUNCION INVERTIR CAMINO
invertir :: Camino -> Camino
invertir [] = []
invertir (x:xs) = invertir(xs)++[x]

--FUNCION QUE ELIMINA CAMINOS REPETIDOS 

eliminarepetidos :: [Camino] -> [Camino]
eliminarepetidos [] = []
eliminarepetidos (x:xs) | elem (x) (xs) == True = eliminarepetidos(xs)
eliminarepetidos (x:xs) = (x):eliminarepetidos(xs)     

--FUNCION EXTENDERCAMINO, SUPONEMOS SIEMPRE QUE N ES UN CAMINO DE POR SI.

extendercaminoizquierda ::Tablero ->  Camino -> Camino
extendercaminoizquierda m (x:xs) |posValida m (fst(x)-1,snd(x)) == True = (fst(x)-1,snd (x)):(x:xs)
extendercaminoizquierda m (x:xs) = (x:xs) 

extendercaminoarriba :: Tablero -> Camino -> Camino
extendercaminoarriba m (x:xs) |posValida m (fst(x),snd(x)-1) == True = (fst(x),snd (x)-1):(x:xs)
extendercaminoarriba m (x:xs) = (x:xs) 

extendercaminoderecha ::Tablero ->  Camino -> Camino
extendercaminoderecha m n |posValida m (fst(head (invertir n))+1,snd(head (invertir n))) == True = invertir(((fst(head (invertir n)))+1,snd (head (invertir n))):(invertir n))
                          |otherwise = n 

extendercaminoabajo :: Tablero -> Camino -> Camino
extendercaminoabajo m n |posValida m (fst(head (invertir n)),snd(head (invertir n))+1) == True = invertir ((fst(head (invertir n)),snd (head (invertir n))+1):(invertir n))
                        |otherwise = n

--CONTIENE VARIOS CAMINOS REPETIDOS EN CASO DE QUE NO PUEDA EXTENDER EN UNA O MAS DIRECCIONES EL CAMINO                                                   
caminosextendidos :: Tablero -> Camino -> [Camino]
caminosextendidos m n = eliminarepetidos[extendercaminoabajo m n ,extendercaminoarriba m n ,extendercaminoizquierda m n,extendercaminoderecha m n]


--ESTA FUNCION TOMA UN CONJUNTO FORMADO POR CAMINOS DE UN TABLERO Y AGREGA TODOS LOS EXTENDIDOS DE CADA UNO PUEDEN APARECER REPETIDOS
extendidosdeconjunto :: Tablero -> [Camino] -> [Camino]  
extendidosdeconjunto m [] = [] 
extendidosdeconjunto m (x:xs)  = (caminosextendidos m (x))++(extendidosdeconjunto m (xs))

caminosHastaLongitudK :: Tablero -> Integer -> [Camino] 
caminosHastaLongitudK m 0 = []
caminosHastaLongitudK m 1 = individuales(coordenadas m )
caminosHastaLongitudK m k = (extendidosdeconjunto m (caminosHastaLongitudK m (k-1)))++(caminosHastaLongitudK m (k-1))

--DA TODOS LOS CAMINOS SIN REPETICIONES
todosLosCaminos :: Tablero -> Conjunto(Camino)
todosLosCaminos m = eliminarepetidos(caminosHastaLongitudK m (longitud(coordenadas m)))
                        
soloFibonacci1 :: Tablero -> [Camino] -> Conjunto(Camino)
soloFibonacci1 m [] =[]
soloFibonacci1 m n | (head n == [] && elem [] soloFibonacci1 m (tail n)) = []:(soloFibonacci1 m (tail n))
                   |(caminoDeFibonacci m tail n )==True = head n:(soloFibonacci1 m tail n)
soloFibonacci1 m n = soloFibonacci1 m (tail n)
                    where n = (todosLosCaminos m )

--Toma todos los caminos de una lista de caminos y devuelve una lista con los caminos que tienen tamaño k
caminosTamanioK :: Conjunto(Camino) -> Integer -> [Camino]
caminosTamanioK [] k = [] 
caminosTamanioK n k  |((longitud(head n))==k) = (head n):(caminosTamanioK (tail n) k)               
                     | otherwise = caminosTamanioK (tail n) k 

--Da todos las secuencias de Fibonacci de tamaño k de un conjunto 

coordenadasDeSecuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto(Camino)
coordenadasDeSecuenciasDeFibonacciDeLongitudK m k = (caminosTamanioK (soloFibonacci m) k)

numeros :: Tablero -> [Camino] -> [[Integer]]
numeros m n | n == [] = [] 
            | otherwise = (numerosDeCamino m (head n)):(numeros m (tail n))

secuencias :: Tablero -> Integer -> [[Integer]]
secuencias m k = numeros m (coordenadasDeSecuenciasDeFibonacciDeLongitudK m k)

 



