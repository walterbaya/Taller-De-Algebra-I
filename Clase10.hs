--Creo un renombre de tipos
type Complejo = (Float , Float)

--Ejercicio 1
suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c,b+d)

--Ejercicio 2
producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c-b*d,a*d+b*c)

--Ejercicio 3 
re :: Complejo -> Float
re (a,b) = a

im :: Complejo -> Float
im (a,b) = b

--Ejercicio 4
modulo :: Complejo -> Float
modulo (a,b) = sqrt(a**2 + b**2)

--Ejercicio 5
--Revisar bien esta implementacion ,mas que nada por un tema de lo que es realmente el argumento
argumento :: Complejo -> Float
argumento (a,b) = atan(b/a)

--Ejercicio 6
conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,-b)
--Ejercicio 7
inverso :: Complejo -> Complejo
inverso (a,b) = conjugado(a/((modulo(a,b))^2),b/(modulo(a,b))^2)
--Ejercicio 8
division :: Complejo -> Complejo -> Complejo
division (a,b) (c,d) = 