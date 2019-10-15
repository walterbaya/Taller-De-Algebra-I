--Guardas == /= son siempre una expresion booleana
-- otherwise es otro tipo de expresion booleana (caso contrario)


f n | n > 5 =  1
    | n > 3 = 2
    | otherwise = 0


-- en este caso f 6 = 1
-- |  indica que comienza una guarda
-- haskell evalua desde arriba hacia abajo IMPORTA EL ORDEN
-- si la escribimos al reves da 2 cuando llamamos a  f 6
-- f n | n>3 = 2
--     | n>5 = 1
--     | otherwise = 0


signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1

-- == es una expresion booleana = es el igual de definicion de funciones

-- La funcion absoluto usa la funcion signo 
-- los negativos siempre se usan entre parentesis cuando se ponen delante de un numero

absoluto n = n * (signo n)

-- Implementacion de maximo 

maximo a b | a >= b = a
           | otherwise = b

-- Reusando maximo a b

maximo3 a b c = maximo (maximo a b) c 

-- Todas las expresiones son de un tipo de dato, es un conjunto de valores y las funciones definidas en esos tipos
-- :t de una expresion dice que tipo de datos tiene la expresion
-- overfloat,  hay un maximo posible que se puede representar cuando me paso me da ese ovefloat
-- f True depende de f el tipo, por ejemplo si fuera de bool a bool f True es de tipo bool
-- :: sirve para indicar de que tipo es f
-- :: bool -> integer -> bool retorna algo de tipo bool y toma dos parametros uno bool y otro integer
-- si hago :t funcion3 me va a dar la signatura de la funcion, o sea ::Integer -> Integer por ejemplo

-- doble x podria ser de integer a integer o de float a float
-- doble True tipa mal da error porque espera un Integer

doble :: Integer -> Integer
doble x = x + x

-- doble 10 es integer o float dependiendo de como defina a doble

cuadruple :: Integer -> Integer
cuadruple n = doble(doble n)
-- Funcion mod da el resto de la division entera 

espar :: Integer -> Bool
espar n | (mod n 2) == 0 = True
        | otherwise = False

-- otra forma espar n = (mod n 2) == 0

esmultiplo :: Integer -> Integer -> Bool
esmultiplo a b = (mod a b) == 0

-- haskell trata de deducir el tipo sino lo ponemos, entonces toma los tipos mas globales posibles
-- Especificar sirve para detectar errores mas facilmente
-- La a es una variable de tipo, la funcion podia recibir algo de cualquier tipo, pero me retorna algo del mismo tipo
-- a puede ser de cualquier tipo
-- si pongo id 1<3 me retorna verdadero, ya que toma un float y me devuelve algo del mismo tipo float
-- triple :: Num a => a -> a significa que va de a en a si a es de tipo numerico o sea float, integer, etc
-- el tipo ord es aquellos a quienes les puede implementar un orden total por ejemplo < >= etc Integer o FLoat
-- == se lo puedo aplicar a cualquier tipo que lo use bool, integer, float, etc

($%) :: Integer -> Integer -> Integer
($%) x y = x + y

 -- () sirven para poder usarlas op1 ($%)op2 o sea que vaya en el medio
 -- La funcion mayor igual recibe parametros de cualquier tipo de ord Flase >= True da algo tipo Bool
 -- Tuplas esta ordenada, cada elemento puede ser de distinto tipo  por ejemplo (1,True) tupla ordenada 
 -- podrian ser de mismo tipo
 -- first (fst) y second funciones para tuplas de dos first recibe una unico parametro que es un par, el primero es a y el segundo b
 -- first (snd) recibe una tupla de tipo  a la primera y la segunda tipo b (no tienen por que ser iguales) y devuelve algo de tipo a
 -- second lo mismo pero devuelve tipo b
 -- una tupla puede ser una coordenada por ejemplo  (1,(2,3))

normaVectorial  :: (Float , Float) -> Float --va de tupla a float
normaVectorial p = sqrt ((fst p) ^ 2 + (snd p) ^ 2)

crearpar :: a -> b -> (a,b)
crearpar a b = (a,b)

invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)

--Esta funcion toma dos duplas de numeros reales y calcula la distancia que hay entre ellas

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (a,b) (c,d) = normaVectorial ((c-a),(d-b))

--resulta importante ver que es conveniente poner (a,b) (c,d) para poder usar norma vectorial

-- Ejercicio 32 iii)  fn + mayus + z para poner >

f1 :: Float -> (Float,Float,Float) 
f1 x = (2*x,x**2,x-7)

--Ejercicio 32 iv)

f2 :: Integer -> Integer
f2 n | mod n 2 == 0 = div n 2 
     | otherwise = n + 1 

--Ejercicio 33 i) 

f3 :: Integer -> Integer
f3 n | mod n 6 == 0 = div (n*n) 2 
     | otherwise = 3*n + 1

--Ejercicio 33 ii)

g :: Integer -> Integer -> Integer 
g n m = n * (m + 1)

--Definicion de la funcion h
 
h n m = f3(g n m)

