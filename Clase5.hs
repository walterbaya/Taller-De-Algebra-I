--Clase numero 5 

--aca la  division 1/n es un float pero yo tengo un int n por lo tanto necesitaria transformar esa n a float 

aproxe :: Integer -> Float
aproxe n | n==0=1
    | otherwise = aproxe(n-1) + 1/fromInteger(factorial n)

e::Float
e = aproxe(100)

--Funcion que calcula la parte entera de un numero

parteEntera :: Float -> Integer
parteEntera x |(-1<=x && x<=1) = 0
              | x>1 = 1 + parteEntera(x-1)
              | x<(-1) = (-1) + parteEntera(x+1)

--algoritmos divisor de euclides para a Entero y para d natural 

division :: Integer -> Integer -> ( Integer , Integer )

division a d | (a < d) && (a >= 0) = (0 , a )
             | a >= d = ( fst(division (a-d) d) + 1 ,snd (division (a-d) d))
             |otherwise = (fst(division (a+d) d) -1 ,snd (division (a+d) d ))
                         
--where sirve para no tener que andar escribiendo todo el vector simplemente lo noto con la l

 

--Suma divisores Hasta
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k| k==0 = 0
                      | mod n k == 0 = k + sumaDivisoresHasta n (k-1)  
                      | otherwise = sumaDivisoresHasta n (k-1)
--Suma divisores 
sumaDivisores :: Integer -> Integer
sumaDivisores n =  sumaDivisoresHasta n n 

--menorDivisor
divisormenordesde :: Integer -> Integer -> Integer
divisormenordesde n k |(k==n)=n
                      |mod n k == 0 = k
                      |otherwise = divisormenordesde n (1+k)
divisormenor :: Integer -> Integer 
divisormenor n = divisormenordesde n 2 

--una posible implementacion de la funcion esPrimo
esPrimo :: Integer -> Bool
esPrimo p | sumaDivisores p == p + 1 = True
          | otherwise = False
-- Creo primeramente las funciones snp y spp
-- para Integers usamos ^
--En el ejercicio 4 uso dos funciones el caso base es 0 ya que al sumarlo no aporta nada
-- y me permite hace la recursion sin tener que evitar poner unos y usar que los casos bases son con 0
-- ¿Podria ponerlo con uno tambien? si, no modifica en nada 

--Otra posible implementacion de esPrimo podria ser usando divisormenor
-- esPrimo p | divisormenor n == 1 = False
--           | otherwise = True

sumaInterior4 :: Float -> Integer -> Integer -> Float
sumaInterior4 q 1 m = q^(fromInteger (m+1))
sumaInterior4 q n m = sumaInterior4 q (n-1) m + q^(fromInteger (n + m))

sumaDoble4 :: Float -> Integer -> Integer -> Float
sumaDoble4 q n 1 = sumaInterior4 q n 1 
sumaDoble4 q n m = sumaInterior4 q n m + sumaDoble4 q n (m-1)

--Ejercicio 3

sumaInterna :: Integer -> Integer -> Integer
sumaInterna n 1 = n 
sumaInterna n m = m^n + sumaInterna n (m-1)

sumaDoble3 :: Integer -> Integer -> Integer
sumaDoble3 1 m = m
sumaDoble3 n m = sumaInterna n m + sumaDoble3 (n-1) m 

--Ejercicio 5

sumaInterna5 :: Integer -> Integer -> Float
sumaInterna5 m 1 = 1/fromInteger m
sumaInterna5 m n = fromInteger n / fromInteger m + sumaInterna5 m (n-1)

sumaDoble5 :: Integer -> Integer -> Float
sumaDoble5 1 n = (fromInteger (n*(n+1)))/2
sumaDoble5 m n = sumaInterna5 m	n + sumaDoble5 (m-1) n
 

