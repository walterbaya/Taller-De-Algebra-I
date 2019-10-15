type Set a = [a]

vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar n conjunto |elem n conjunto == True = conjunto
                   |otherwise = n:conjunto

--Ejercicio 1a

incluido :: Set Integer -> Set Integer -> Bool
incluido a b | a == [] = True
             | elem (head a) b == False = False
             | otherwise = incluido(tail a) b 

--Ejercicio 2a
iguales :: Set Integer -> Set Integer -> Bool
iguales a b = (incluido a b) && (incluido b a)

--Funcion agregarAtodos

agregarAtodos :: Integer -> Set (Set Integer) ->  Set (Set Integer)
agregarAtodos x n |tail n == [] = [agregar x (head n)]
                  |elem x (head n) == True = (head n):agregarAtodos x (tail n)
                  |otherwise =  agregar x (head n):agregarAtodos x (tail n)

--Funcion partes

partes :: Integer -> Set (Set Integer)
partes n | n==0=[[]]
         | otherwise = (agregarAtodos n (partes(n-1)))++partes(n-1) 

--Funcion producto cartesiano 

--Funcion agregadoInterno
agregadoInterno :: Integer -> Set Integer -> Set(Integer,Integer)
agregadoInterno n m | tail m == [] = [(n,head m)]
                    | otherwise = (n,head m): agregadoInterno n (tail m) 

--Esta funcion contiene repeticiones 

agregadoTotal :: Set Integer -> Set Integer -> Set(Integer,Integer)
agregadoTotal n m | tail n == [] = agregadoInterno (head n) m 
                  | otherwise = (agregadoInterno (head n) m) ++ agregadoTotal (tail n) m


--Ejercicio (eliminar repetidos)

eliminarepes :: Set (Integer,Integer) -> Set (Integer,Integer)
eliminarepes n | tail n == [] = [(head n)]
               | elem (head n) (tail n) == True = eliminarepes (tail n)
               | otherwise = (head n) : eliminarepes (tail n)
 
agregadototal :: Set Integer -> Set Integer -> Set(Integer,Integer)
agregadototal n m = eliminarepes (agregadoTotal n m)  

            

