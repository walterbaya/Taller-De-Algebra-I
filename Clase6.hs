-- Ejercicio 1
-- Iguales esta bien definida, ya que si meto dos parametros iguales me imprime true, de otra forma si meti otra cosa
--me dice false
--Sin embargo aca me aparece conflictin definition for x esto se debe a que x esta definiendose dos veces en iguales x x  
-- no puedo definir dos veces la misma variable no esta permitido en pattern matching hacer f x x por ejemplo
--caso de factorial se debe a patrones de tipo n+k que no son soportados en la version moderna de haskell pero solian usarse en el 98

--Ejercicio 2 

--YLOGICO

ylogico :: Bool -> Bool -> Bool
ylogico True True = True
ylogico False False = True
ylogico _ _ = False

--OLOGICO

ologico :: Bool -> Bool -> Bool
ologico False False = False
ologico _ _ = True

--IMPLICA

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--SUMA GAUSSIANA

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

--AlgunoEsCero
algunoEsCero :: (Integer,Integer,Integer) -> Bool
algunoEsCero (0,y,z) = True
algunoEsCero (x,0,z) = True
algunoEsCero (x,y,0) = True
algunoEsCero (_,_,_) = False

--productointerno
productointerno :: (Float,Float)->(Float,Float) -> Float
productointerno (x1,y1) (x2,y2) = x1*x2 + y1*y2 

--Es necesario importar estas funciones para desarrollar las siguientes
sdh :: Integer -> Integer -> Integer
sdh n k| k==0 = 0
       | mod n k == 0 = sdh n (k-1) + k 
       | otherwise = sdh n (k-1)
 
sumaDivisores :: Integer -> Integer
sumaDivisores n =  sdh n n 

esPrimo :: Integer -> Bool
esPrimo p 
          | sumaDivisores p == p + 1 = True
          | otherwise = False

ecsh :: Integer -> Integer -> Bool
ecsh _ 1 = False
ecsh 1 _ = False
ecsh 2 _ = False
ecsh 3 _ = False
ecsh n k | (esPrimo k == True  && esPrimo (n-k)==True) = True 
ecsh n k = ecsh n (k-1)

--Ejercicio 1

esSumaDeDosPrimos  :: Integer -> Bool
esSumaDeDosPrimos n = ecsh n n 

--Ejercicio 2

goldbach :: Integer -> Bool
goldbach 2 = True
goldbach n | (mod n 2 == 0) && (esSumaDeDosPrimos n == False) = False
           | (mod n 2 == 0) && (esSumaDeDosPrimos n == True) = goldbach(n-2) 
goldbach n = goldbach (n-1)

--Ejercicio 3

sumadigitos :: Integer -> Integer
sumadigitos n | n < 10 = n 
              | otherwise = sumadigitos ( div n 10) + mod n 10

--Ejercicio 4 

digitosIguales :: Integer -> Bool
digitosIguales n | n<10 = True
digitosIguales n | (mod n 10 == mod (div n 10) 10) == True = digitosIguales (div n 10) 
digitosIguales n = False 
