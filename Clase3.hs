--unidades: dado un entero, devuelve el dı́gito de las unidades del número (el dı́gito menos significativo).

unidades :: Integer -> Integer
unidades n = mod n 10 

sumaunidades3 :: Integer -> Integer -> Integer -> Integer
sumaunidades3 a b c = (unidades a) + (unidades b) + (unidades c)

espar :: Integer -> Bool 
espar n | mod n 2 == 0 = True  
        | otherwise = False

esImpar :: Integer -> Bool
esImpar n = not (espar n)

--todosImpares: dados 3 números enteros determina si son todos impares. 

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares a b c = (esImpar a)&& (esImpar b) && (esImpar c)
                   

--alMenosUnImpar: dados 3 números enteros determina si al menos uno de ellos es impar.

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c = ((esImpar a) || (esImpar b) || (esImpar c)) 

--alMenosDosImpares: dados 3 números enteros determina si al menos dos de ellos son impares.

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares a b c | (esImpar a) && (esImpar b) == True = True
                        | (esImpar a) && (esImpar c) == True = True
                        | (esImpar b) && (esImpar c) == True = True
                        | otherwise = False

--alMenosDosPares: dados 3 números enteros determina si al menos dos de ellos son pares.
-- si no hay al menos dos impares es porque inevitablemente hay dos pares al menos ya que eso quiere decir que hay a lo sumo un impar o ninguno

alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares a b c = not(alMenosDosImpares a b c)

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod x y == 0

--alMenosUnMultiploDe: dados 3 números enteros determina si alguno de los primeros dos es múltiplo del tercero

alMenosUnMultiploDe :: Integer -> Integer -> Integer -> Bool
alMenosUnMultiploDe a b c = (esMultiploDe a c) || (esMultiploDe b c)

--Ejercicio 8
--rehusando los ejercicios anteriores...

r1 :: Integer -> Integer -> Bool
r1 a b = (todosImpares a b 5) || not(alMenosUnImpar a b 2)

r2 :: Integer -> Integer -> Bool
r2 a b = esMultiploDe (2*a + 3*b) 5

r3 :: Integer -> Integer -> Bool
r3 a b = (unidades a /= unidades b) && (unidades a /= unidades (a*b)) && (unidades b /= unidades (a*b))

--Ejercicio 9

equivalencia :: Float -> Float -> Bool
equivalencia a b = ((a < 3) && (b < 3)) || ((a >= 3) && (b >=3 ))
                 

equivalencia2 :: Float -> Float -> Bool
equivalencia2 a b = ((a < 3) && (b < 3)) || (( a >= 3 && a < 7 ) && ( b >= 3 && b < 7)) || ((a >= 7) && (b >=7 ))

--Ej 11 

--a 

--Forma parte del contrato entre programadores que los valores de las duplas no pueden ser 0 ninguno

equivalencia11a :: (Float,Float) -> (Float,Float) -> Bool
equivalencia11a (a,b) (p,q) = ((a/p)==(b/q))
--Implementar la función sc :: Integer -> Integer definida por(0 si n = 0 y sc(n − 1) + n^2 si no

sc :: Integer -> Integer 
sc n | n == 0 = 0
     | n /= 0 = sc(n-1) + n*n 

--Implementar la función fib :: Integer -> Integer que devuelve el i-ésimo número de
--Fibonacci. Recordar que la sucesión de Fibonacci se define como: 
--si n = 0 es 0 y 1 si n = 1 o  fib(n − 1) + fib(n − 2) en otro caso

fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

--defino el factorial

factorial :: Integer -> Integer
factorial n |n == 0 = 1 
            |n > 0 = n * factorial (n-1)

a1 :: Integer -> Integer
a1 n | n == 1 = 2
     | otherwise = 2*(n-1)*a1(n-1) + (2^n)*factorial(n-1)

a2 :: Integer -> Integer
a2 n | n == 1 = 1
     | n == 2 = 2
     | otherwise = (n-2)*a2(n-1) + 2*(n-1)*a2(n-2)

a3 :: Integer -> Integer
a3 n | n == 1 = (-3)
     | n == 2 = 6
     | mod n 2 == 0 = a3(n-1) + 2 * a3(n-2) + 6
     | otherwise = (-1 * a3(n-1)) - 3              

 




